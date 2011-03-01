(ns ^{:slix true}
  slix.planter
  (:use [sevenri config core event log slix ui utils]
        [slix.planter core]))

(defn opened
  [event]
  (set-slix-visible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti built?
  (fn [obj] (class obj)))

(defmethod built? clojure.lang.PersistentArrayMap
  [m]
  (is-project-built? (:project-name m)))

(defmethod built? :default
  [sym]
  (built? {:project-name sym}))

;;;;

(defmulti build?
  (fn [obj] (class obj)))

(defmethod build? clojure.lang.PersistentArrayMap
  [m]
  (build-project? (:project-name m)))

(defmethod build? :default
  [sym]
  (build? {:project-name sym}))

;;;;

(defmulti get-jars
  (fn [obj] (class obj)))

(defmethod get-jars clojure.lang.PersistentArrayMap
  [m]
  (get-project-all-jars (:project-name m)))

(defmethod get-jars :default
  [sym]
  (get-jars {:project-name sym}))
