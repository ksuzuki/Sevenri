(ns slix.planter.core
  (:use [sevenri config log utils]
        clojure.java.shell)
  (:import [java.io File FileFilter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn project-name-to-map
  [sym & optms]
  (let [dir (File. (nssym2safepath sym))
        pdir (.getParent dir)
        pname (if pdir (.getName dir) sym)]
    (apply merge {:dir dir :parent-dir pdir :name pname} optms)))

(defn get-project-parent-path
  [pmap]
  (File. (get-projects-dir) (str (:parent-dir pmap))))

(defn get-project-path
  [pmap]
  (File. (get-projects-dir) (str (:dir pmap))))

(defn get-project-file
  [pmap]
  (File. (get-project-path pmap) "project.clj"))

(defmulti project-exists?
  (fn [obj] (class obj)))

(defmethod project-exists? clojure.lang.PersistentArrayMap
  [pmap]
  (let [path (get-project-path pmap)
        pclj (get-project-file pmap)]
    (every? #(.exists %) [path pclj])))

(defmethod project-exists? :default
  [sym]
  (let [pmap (project-name-to-map sym)]
    (project-exists? pmap)))

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
    (sh-lein pmap cmds)))

(defn create-project
  [sym]
  (when-not (project-exists? sym)
    (let [pmap (project-name-to-map sym)
          pdir (get-project-parent-path pmap)
          run? (if (.exists pdir)
                 true
                 (and (.mkdirs pdir) (.exists pdir)))]
      (when run?
        (sh-lein (assoc pmap :dir (:parent-dir pmap)) ["new" (:name pmap)])))))

(defn get-project-deps
  [sym]
  (run-lein ["deps"] sym))

(defn compile-project
  [sym]
  (run-lein ["compile"] sym))

(defn test-project
  [sym]
  (run-lein ["test"] sym))

(defn create-project-jar
  [sym]
  (run-lein ["jar"] sym))

(defn clean-project
  [sym]
  (run-lein ["clean"] sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project-jars
  [sym]
  (let [pmap (project-name-to-map sym)]
    (when (project-exists? pmap)
      (find-files '.jar (get-project-path pmap)))))

(defn get-project-artifact-jar
  [sym]
  (let [pmap (project-name-to-map sym)]
    (when (project-exists? pmap)
      (let [prjct (read-string (slurp (get-project-file pmap) :encoding "UTF-8"))
            pname (second prjct)
            pvrsn (nth prjct 2)]
        (File. (get-project-path pmap) (str pname "-" pvrsn ".jar"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-project-built?
  [sym]
  (and (project-exists? sym)
       (.exists (get-project-artifact-jar sym))))

(defn build-project?
  [sym]
  (if (project-exists? sym)
    (do
      (create-project-jar sym)
      (is-project-built? sym))
    false))
