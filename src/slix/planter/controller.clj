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

;;;;


(defn save-project-name
  [slix]
  (let [pn (get-project-name slix)]
    (with-create-sn-get-dir
      (let [sf (get-sid-slix-state-file slix)]
        (spit sf pn :encoding "UTF-8")))))

(defn load-project-name
  [slix]
  (let [sf (get-sid-slix-state-file slix)]
    (when (.exists sf)
      (slurp sf :encoding "UTF-8"))))

(defn save-state
  [slix]
  (save-project-name slix))

(defn load-state
  [slix]
  {:project (load-project-name slix)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-project-used
  [sym]
  (when-let [kvs (xref-with sym)]
    (when-first [kv (filter #(= (second %) *xref-planter-project*) kvs)]
      (first kv))))

(defn get-unused-project
  []
  (when-first [pn (filter #(nil? (is-project-used %)) (sort (keys (get-project-name-config-map))))]
    pn))

(defn do-project-or-close
  [slix]
  (if-let [pn (:project (or (slix-args slix) (load-state slix)))]
    (if-let [slx (is-project-used pn)]
      (do
        (invoke-later slx #(.toFront (slix-frame slx)))
        (close-slix slix)
        nil)
      (do
        (invoke-later slix #(.toFront (slix-frame slix)))
        (set-project-name slix pn)))
    (if-let [pn (get-unused-project)]
      (do
        (invoke-later slix #(.toFront (slix-frame slix)))
        (set-project-name slix pn))
      (do
        (close-slix slix)
        nil))))

(defn is-project-busy?
  "FIX ME"
  [frame]
  false)
