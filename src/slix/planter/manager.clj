(ns slix.planter.manager
  (:use [slix.planter.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query I/F for manager

(defmulti ready?
  (fn [obj] (class obj)))

(defmethod ready? clojure.lang.PersistentArrayMap
  [m]
  (is-manager-ready? (:slix-name m)))

(defmethod ready? :default
  [slix-name]
  (ready? {:slix-name slix-name}))

;;;;

(defmulti setup?
  (fn [obj] (class obj)))

(defmethod setup? clojure.lang.PersistentArrayMap
  [m]
  (setup-manager? (:slix-name m)))

(defmethod setup? :default
  [slix-name]
  (setup? {:slix-name slix-name}))

;;;;

(defmulti shutdown
  (fn [obj] (class obj)))

(defmethod shutdown clojure.lang.PersistentArrayMap
  [m]
  (shutdown-manager (:slix-name m)))

(defmethod shutdown :default
  [slix-name]
  (shutdown {:slix-name slix-name}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query I/F for project

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

(defmulti build?
  (fn [obj] (class obj)))

(defmethod build? clojure.lang.PersistentArrayMap
  [m]
  (build-project? (:slix-name m)))

(defmethod build? :default
  [slix-name]
  (build? {:slix-name slix-name}))

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

(defn build-and-run
  ([m]
     (build-project-and-run (:slix-name m) (:name m) (:arguments m)))
  ([slix-name name args]
     (build-and-run {:slix-name slix-name :name name :arguments args})))
