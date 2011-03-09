;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Sevenri, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns slix.planter.core
  (:use [sevenri config core log slix utils]
        [slix.planter defs]
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

;;;;

(defmulti get-project-parent-path
  (fn [obj] (class obj)))

(defmethod get-project-parent-path clojure.lang.PersistentArrayMap
  [pmap]
  (File. (get-project-dir) (str (:parent-dir pmap))))

(defmethod get-project-parent-path :default
  [sym]
  (get-project-parent-path (project-name-to-map sym)))

;;;;

(defmulti get-project-path
  (fn [obj] (class obj)))

(defmethod get-project-path clojure.lang.PersistentArrayMap
  [pmap]
  (File. (get-project-dir) (str (:dir pmap))))

(defmethod get-project-path :default
  [sym]
  (get-project-path (project-name-to-map sym)))

;;;;

(defmulti get-project-config-file
  (fn [obj] (class obj)))

(defmethod get-project-config-file clojure.lang.PersistentArrayMap
  [pmap]
  (get-project-file (str (:symbol pmap) ".project")))

(defmethod get-project-config-file :default
  [sym]
  (get-project-config-file (project-name-to-map sym)))

;;;;

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

;;;;

(defmulti remove-project
  (fn [obj] (class obj)))

(defmethod remove-project clojure.lang.PersistentArrayMap
  [pmap]
  (trash-dir? (get-project-path pmap) (get-project-dir)))

(defmethod remove-project :default
  [sym]
  (remove-project (project-name-to-map sym)))

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

(defn setup-slix-project?
  [pmap]
  (letfn [(modify-project []
            (let [pf (get-project-file (:symbol pmap) 'project)]
              (if (.exists pf)
                (try
                  (spit pf (format (str "(defproject %s \"0.1.0-SNAPSHOT\"\n"
                                        "  :description \"%s project\")\n")
                                   (:symbol pmap) (:symbol pmap))
                        :encoding "UTF-8")
                  true
                  (catch Exception e
                    false))
                false)))
          (write-proj-clj []
            (let [pn (:name pmap)
                  sp (File. (get-project-path pmap) (str (File. "src" pn)))
                  fc (File. sp "core.clj")
                  dp (File. (get-project-path pmap) (str (File. (File. "src" "slix") pn)))
                  fp (File. dp "proj.clj")]
              (if (.exists fc)
                (try
                  (spit fc (format "(ns %s.proj)\n" (:symbol pmap)) :encoding "UTF-8")
                  (.mkdirs dp)
                  (.renameTo fc fp)
                  (.delete sp)
                  true
                  (catch Exception e
                    false))
                false)))
          (write-test-clj []
            (let [pn (str (File. (:name pmap) "test"))
                  sp (File. (get-project-path pmap) (str (File. "test" pn)))
                  fc (File. sp "core.clj")
                  dp (File. (get-project-path pmap) (str (File. (File. "test" "slix") pn)))
                  fp (File. dp "proj.clj")]
              (if (.exists fc)
                (try
                  (spit fc (format (str "(ns %s.test.proj\n"
                                        "  (:use [%s.proj] :reload)\n"
                                        "  (:use [clojure.test]))\n\n"
                                        "(deftest replace-me ;; FIXME: write\n"
                                        "  (is false \"No tests have been written.\"))\n")
                                   (:symbol pmap) (:symbol pmap))
                        :encoding "UTF-8")
                  (.mkdirs dp)
                  (.renameTo fc fp)
                  (.delete sp)
                  (.delete (.getParentFile sp))
                  true
                  (catch Exception e
                    false))
                false)))]
    (and (modify-project) (write-proj-clj) (write-test-clj))))

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
              (assoc rm :slix-project true :err (if (setup-slix-project? pmap)
                                                  ""
                                                  "setup-slix-project? failed"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn build-slix-project-and-run
  [fqsn nm args]
  (if (build-project? fqsn)
    (let [sn (get-slix-name-from-fqns fqsn)]
      (if args
        (if nm
          (open-slix-with-args args sn nm)
          (open-slix-with-args args sn))
        (if nm
          (open-slix sn nm)
          (open-slix sn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-manager-ready?
  [_]
  (is-project-built? 'slix.planter))

(defn setup-manager?
  [_]
  (build-project? 'slix.planter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-out-ps
  []
  (let [baos (java.io.ByteArrayOutputStream.)
        ops (java.io.PrintStream. baos)]
    [baos ops]))

(defmacro def-out-ps
  [n]
  (let [[baos ops] (get-out-ps)
        baosn (symbol (format "baos%d" n))
        opsn (symbol (format "ops%d" n))]
    `(do
       (def ~baosn ~baos)
       (def ~opsn ~ops))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn debug-in-repl
  []
  (let [ds 'slix.planter.debug]
    (require ds)
    (in-ns ds)))
