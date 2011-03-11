(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter core defs io])
  (:import (java.io PipedInputStream PipedOutputStream PrintStream)
           (java.io BufferedReader InputStreamReader)))

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

(defn debug-in-repl
  []
  (let [ds 'slix.planter.debug]
    (require ds)
    (in-ns ds)))

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

(defn force-lein-agent-free
  [slix-or-frame]
  (restart-agent (get-lein-agent slix-or-frame) false))

(defn is-lein-agent-busy?
  [slix-or-frame]
  @(get-lein-agent slix-or-frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn send-task-to-lein-agent
  [slix-or-frame task]
  (lein-agent-busy slix-or-frame)
  (send (get-lein-agent slix-or-frame) task))

(defn create-lein-task
  [slix-or-frame out-txtpn proj-name task-name & task-args]
  (let [slix (get-slix slix-or-frame)
        opwd (str (get-project-path (project-name-to-map proj-name)))
        task (str task-name)
        args (seq (map str task-args))
        ltcl (.getContextClassLoader (Thread/currentThread))
        ;;
        ant-pos (PipedOutputStream.)
        ant-prs (PrintStream. ant-pos)
        ant-bfr (BufferedReader. (InputStreamReader. (PipedInputStream. ant-pos)))
        [lein-baos lein-oprs] (get-out-ps)
        ;;
        doc (.getDocument out-txtpn)
        eof (fn [d] (max 0 (dec (.getLength doc))))
        ins (fn [s]
              (.insertString doc (eof doc) s nil)
              (.setCaretPosition out-txtpn (eof doc)))]
    (fn [_]
      ;; Print a start of task msg and start an ant output msg printer.
      (invoke-later slix #(ins (str "=== " proj-name ": " task " ===\n")))
      (future (loop [l (.readLine ant-bfr)]
                (if l
                  (do
                    (when-not (empty? l)
                      (invoke-later slix #(ins (str l "\n"))))
                    (recur (.readLine ant-bfr)))
                  #_(lg "eof on ant-bfr"))))
      ;; Run this lein task.
      (let [ct (Thread/currentThread)
            cl (.getContextClassLoader ct)]
        ;; Inherit the planter's class loader or lein crashes.
        (.setContextClassLoader ct ltcl)
        (binding [*ns* nil]
          (in-ns 'leiningen.core)
          (binding [*out* (java.io.OutputStreamWriter. lein-oprs)
                    *original-pwd* opwd
                    lancet/ant-project (lancet/get-ant-project ant-prs ant-prs)]
            (try
              (let [result (apply -main task args)]
                #_(lg "lein result:" result))
              (catch Exception e
                (log-exception e)))))
        ;; Restore the original cl or mysterious ThreadDeath occurs.
        (.setContextClassLoader ct cl))
      ;; Close the ant output pipe, which should end the ant output msg
      ;; printer started above. Then print out the lein msg.
      (.close ant-pos)
      (let [lms (.toString lein-baos)]
        (invoke-later slix #(ins (str (if (empty? lms) "#\n\n" (str lms "#\n\n"))))))
      ;; This lein task is finished. Return false to signify the lein agent
      ;; is NOT busy.
      false)))

(defn do-lein
  [frame out-txtpn proj-name cmd]
  (let [task (create-lein-task frame out-txtpn proj-name cmd)]
    (send-task-to-lein-agent frame task)))
