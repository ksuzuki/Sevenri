(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter core defs io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-not (find-ns 'lancet)
  (create-ns 'lancet)
  (intern 'lancet 'ant-project nil)
  (intern 'lancet 'get-ant-project (fn [& args])))

(when-not (find-ns 'leiningen.core)
  (create-ns 'leiningen.core)
  (intern 'leiningen.core '*original-pwd* "")
  (intern 'leiningen.core '-main (fn [& args]))
  (intern 'leiningen.core '*planter-init* false))

(use 'leiningen.core)

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

;;;;

(defn create-lein-task
  [slix-or-frame out-txtpn proj-name task-name & task-args]
  (let [slix (get-slix slix-or-frame)
        opwd (str (get-project-path (project-name-to-map proj-name)))
        task (str task-name)
        args (seq (map str task-args))
        ltcl (.getContextClassLoader (Thread/currentThread))
        [lein-baos lein-ops] (get-out-ps)
        [ant-baos ant-ops] (get-out-ps)]
    (fn [_]
      (let [ct (Thread/currentThread)
            cl (.getContextClassLoader ct)]
        ;; Inherit the planter's class loader or lein crashes.
        (.setContextClassLoader ct ltcl)
        (binding [*ns* nil]
          (in-ns 'leiningen.core)
          (binding [*out* (java.io.OutputStreamWriter. lein-ops)
                    *original-pwd* opwd
                    lancet/ant-project (lancet/get-ant-project ant-ops ant-ops)]
            (try
              (let [result (apply -main task args)]
                (lg "result:" result))
              (catch Exception e
                (log-exception e)))))
        ;; Restore the original cl or mysterious ThreadDeath occurs.
        (.setContextClassLoader ct cl))
      ;;
      (let [doc (.getDocument out-txtpn)
            off (max 0 (dec (.getLength doc)))
            ams (.toString ant-baos)
            lms (.toString lein-baos)]
        (invoke-later slix #(do
                              (.insertString doc
                                             off
                                             (str "=== " proj-name ": " task " ===\n"
                                                  "[ant output]\n" (when-not (empty? ams) ams)
                                                  "[lein output]\n" (if (empty? lms) "\n" lms)
                                                  "\n")
                                             nil)
                              (.setCaretPosition out-txtpn (max 0 (dec (.getLength doc)))))))
      false)))

(defn do-lein-compile
  [frame out-txtpn proj-name]
  (let [task (create-lein-task frame out-txtpn proj-name "compile")]
    (send-task-to-lein-agent frame task)))
