(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter defs io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti get-project-name
  (fn [obj] (class obj)))

(defmethod get-project-name clojure.lang.PersistentArrayMap
  [slix]
  (when (is-slix? slix)
    (when-let [kvs (xref-with slix)]
      (when-first [kv (filter #(= (first %) *xref-planter-project*) kvs)]
        (second kv)))))

(defmethod get-project-name javax.swing.JFrame
  [frame]
  (get-project-name (get-slix frame)))

;;;;

(defmulti set-project-name
  (fn [obj sym] (class obj)))

(defmethod set-project-name clojure.lang.PersistentArrayMap
  [slix sym]
  (when (is-slix? slix)
    (add-to-xref slix *xref-planter-project* sym)
    sym))

(defmethod set-project-name javax.swing.JFrame
  [frame sym]
  (set-project-name (get-slix frame) sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-project-used
  [sym]
  (when-let [kvs (xref-with sym)]
    (when-first [kv (filter #(= (second %) *xref-planter-project*) kvs)]
      (first kv))))

(defn get-unused-project
  []
  (when-first [pn (filter #(nil? (is-project-used %))
                          (sort (keys (get-project-name-config-map))))]
    pn))

(defn is-project-busy?
  "FIX ME"
  [frame]
  false)
