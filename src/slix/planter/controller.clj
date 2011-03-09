(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter core defs io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project-name
  [slix-or-frame]
  (when-let [slix (get-slix slix-or-frame)]
    (when-let [kvs (xref-with slix)]
      (when-first [kv (filter #(= (first %) *xref-planter-project*) kvs)]
        (second kv)))))

(defn set-project-name
  [slix-or-frame sym]
  (when-let [slix (get-slix slix-or-frame)]
    (add-to-xref slix *xref-planter-project* sym)
    sym))

(defn set-title
  ([sym]
     (set-title sym *slix*))
  ([sym slix]
     (set-slix-title (format "%s - %s" (slix-name slix) (str sym)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-lein-agent
  [slix-or-frame]
  (if-let [slix (get-slix slix-or-frame)]
    (put-slix-prop slix :lein-agent (agent false))
    (throw (IllegalArgumentException. "neither slix or frame"))))

(defn get-lein-agent
  [slix-or-frame]
  (if-let [slix (get-slix slix-or-frame)]
    (if-let [la (get-slix-prop slix :lein-agent)]
      la
      (throw (IllegalStateException. "no lein-agent")))
    (throw (IllegalArgumentException. "neither slix or frame"))))

(defn lein-agent-busy
  [slix-or-frame]
  (send (get-lein-agent slix-or-frame) (fn [_] true)))

(defn lein-agent-free
  [slix-or-frame]
  (send (get-lein-agent slix-or-frame) (fn [_] false)))

(defn is-lein-agent-busy?
  "FIX ME"
  [slix-or-frame]
  @(get-lein-agent slix-or-frame))

(defn send-task-to-lein-agent
  [slix-or-frame task]
  (lein-agent-busy slix-or-frame)
  (send (get-lein-agent slix-or-frame) task))
