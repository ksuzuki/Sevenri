(ns ^{:slix true}
  slix.planter
  (:use [sevenri config core event log slix ui utils]
        [slix.planter core]))

(defn opened
  [event]
  (set-slix-visible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; I/F for slix

(defmulti exists?
  (fn [obj] (class obj)))

(defmethod exists? clojure.lang.PersistentArrayMap
  [m]
  (project-exists? (:slix-name m)))

(defmethod exists? :default
  [slix-name]
  (exists? {:slix-name slix-name}))

;;;;

(defmulti built?
  (fn [obj] (class obj)))

(defmethod built? clojure.lang.PersistentArrayMap
  [m]
  (is-project-built? (:slix-name m)))

(defmethod built? :default
  [slix-name]
  (built? {:slix-name slix-name}))

;;;;

(defmulti get-jars
  (fn [obj] (class obj)))

(defmethod get-jars clojure.lang.PersistentArrayMap
  [m]
  (get-project-all-jars (:slix-name m)))

(defmethod get-jars :default
  [slix-name]
  (get-jars {:slix-name slix-name}))

;;;;

(defmulti build-and-run
  (fn [obj] (class obj)))

(defmethod build-and-run clojure.lang.PersistentArrayMap
  [m]
  (build-slix-project-and-run (:slix-name m) (:name m) (:arguments m)))

(defmethod build-and-run :default
  [slix-name name args]
  (build-and-run {:slix-name slix-name :name name :arguments args}))
