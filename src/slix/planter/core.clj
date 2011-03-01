(ns slix.planter.core
  (:use [sevenri config core log utils]
        clojure.java.shell)
  (:import [java.io File FileFilter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn project-name-to-map
  [sym & optms]
  (let [dir (File. (nssym2safepath sym))
        pdir (.getParentFile dir)
        pname (if pdir
                (.getName dir)
                (str sym))]
    (apply merge {:symbol sym :dir dir :parent-dir pdir :name pname} optms)))

(defmulti get-project-parent-path
  (fn [obj] (class obj)))

(defmethod get-project-parent-path clojure.lang.PersistentArrayMap
  [pmap]
  (File. (get-project-dir) (str (:parent-dir pmap))))

(defmethod get-project-parent-path :default
  [sym]
  (get-project-parent-path (project-name-to-map sym)))

(defmulti get-project-path
  (fn [obj] (class obj)))

(defmethod get-project-path clojure.lang.PersistentArrayMap
  [pmap]
  (File. (get-project-dir) (str (:dir pmap))))

(defmethod get-project-path :default
  [sym]
  (get-project-path (project-name-to-map sym)))

(defmulti get-project-config-file
  (fn [obj] (class obj)))

(defmethod get-project-config-file clojure.lang.PersistentArrayMap
  [pmap]
  (get-project-file (str (:symbol pmap) ".project")))

(defmethod get-project-config-file :default
  [sym]
  (get-project-config-file (project-name-to-map sym)))

(defmulti project-exists?
  (fn [obj] (class obj)))

(defmethod project-exists? clojure.lang.PersistentArrayMap
  [pmap]
  (let [path (get-project-path pmap)
        pclj (get-project-config-file pmap)]
    (every? #(.exists %) [path pclj])))

(defmethod project-exists? :default
  [sym]
  (project-exists? (project-name-to-map sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti is-slix-project?
  (fn [obj] (class obj)))

(defmethod is-slix-project? clojure.lang.PersistentArrayMap
  [pmap]
  (if (re-matches (re-pattern (str "^" (get-default :src :slix :dir-name) "/.*"))
              (str (:dir pmap)))
    true
    false))

(defmethod is-slix-project? :default
  [sym]
  (is-slix-project? (project-name-to-map sym)))

;;;;

(defn write-slix-project-core-file?
  [pmap]
  (let [p (File. (get-project-path pmap) (str (File. "src" (:name pmap))))
        f (File. p "core.clj")]
    (if (.exists f)
      (try
        (spit f (format "(ns project.%s.core)\n" (:symbol pmap)) :encoding "UTF-8")
        true
        (catch Exception e
          false))
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sh-lein
  [pmap args]
  (let [shdir (get-project-path pmap)]
  (with-sh-dir shdir
    #_(lg "pmap:" pmap "args:" args "shdir:" shdir)
    (apply sh "lein" args))))

(defn run-lein
  [cmds sym]
  (let [pmap (project-name-to-map sym)]
    (when (project-exists? pmap)
      (assoc (sh-lein pmap cmds) :slix-project (is-slix-project? pmap)))))

;;;;

(defn create-project
  [sym]
  (when-not (project-exists? sym)
    (let [pmap (project-name-to-map sym)
          pdir (get-project-parent-path pmap)
          run? (if (.exists pdir)
                 true
                 (and (.mkdirs pdir) (.exists pdir)))]
      (when run?
        (let [rm (sh-lein (assoc pmap :dir (:parent-dir pmap)) ["new" (:name pmap)])]
          (when (and (zero? (:exit rm))
                     (empty? (:err rm))
                     (project-exists? pmap))
            (if (is-slix-project? pmap)
              (assoc rm :slix-project true :err (if (write-slix-project-core-file? pmap)
                                                  ""
                                                  "write-slix-project-core-file-failed"))
              (assoc rm :six-project false))))))))

(defmacro planter-new
  [sym]
  `(create-project ~sym))

;;;;

(defn get-project-deps
  [sym]
  (run-lein ["deps"] sym))

(defmacro planter-deps
  [sym]
  `(get-project-deps ~sym))

;;;;

(defn compile-project
  [sym]
  (run-lein ["compile"] sym))

(defmacro planter-compile
  [sym]
  `(compile-project ~sym))

;;;;

(defn test-project
  [sym]
  (run-lein ["test"] sym))

(defmacro planter-test
  [sym]
  `(test-project ~sym))

;;;;

(defn create-project-jar
  [sym]
  (run-lein ["jar"] sym))

(defmacro planter-jar
  [sym]
  `(create-project-jar ~sym))

;;;;

(defn clean-project
  [sym]
  (run-lein ["clean"] sym))

(defmacro planter-clean
  [sym]
  `(clean-project ~sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project-jar
  [sym]
  (let [pmap (project-name-to-map sym)]
    (when (project-exists? pmap)
      (let [prjct (read-string (slurp (get-project-config-file pmap) :encoding "UTF-8"))
            pname (second prjct)
            pvrsn (nth prjct 2)]
        (File. (get-project-path pmap) (str pname "-" pvrsn ".jar"))))))

(defn get-project-all-jars
  [sym]
  (let [pmap (project-name-to-map sym)]
    (when (project-exists? pmap)
      (seq (find-files '.jar (get-project-path pmap))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-project-built?
  [sym]
  (and (project-exists? sym)
       (.exists (get-project-jar sym))))

(defn build-project?
  [sym]
  (if (project-exists? sym)
    (do
      (create-project-jar sym)
      (is-project-built? sym))
    false))
