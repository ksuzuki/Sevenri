(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter core defs io tohandler])
  (:import (java.awt Color)
           (java.io PipedInputStream PipedOutputStream PrintStream)
           (java.io BufferedReader File InputStreamReader)
           (java.net URL URLClassLoader)
           (javax.swing.text SimpleAttributeSet StyleConstants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *attr-hdr* (let [a (SimpleAttributeSet.)]
                  (StyleConstants/setForeground a Color/blue)
                  a))

(def *attr-wrn* (let [a (SimpleAttributeSet.)]
                  (StyleConstants/setForeground a Color/red)
                  a))

(def *attr-ok* (let [a (SimpleAttributeSet.)]
                 (StyleConstants/setForeground a (Color. 0 16r99 0))
                 a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-not (find-ns 'lancet)
  (create-ns 'lancet)
  (intern 'lancet 'ant-project nil))

(when-not (find-ns 'lancet.core)
  (create-ns 'lancet.core)
  (intern 'lancet.core 'ant-project nil))

(when-not (find-ns 'leiningen.core)
  (create-ns 'leiningen.core)
  (intern 'leiningen.core '*original-pwd* "")
  (intern 'leiningen.core '*eval-in-lein* false)
  (intern 'leiningen.core '*exit* false)
  (intern 'leiningen.core '*test-summary* nil)
  (intern 'leiningen.core '-main (fn [& args]))
  (intern 'leiningen.core 'get-ant-project (fn [& args])))

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

(defn is-a-jutest-task?
  [task]
  (if (re-matches #"^jutest.*" task)
    true
    false))

(defn create-lein-task
  [slix-or-frame out-txtpn proj-name task-name & task-args]
  (let [slix (get-slix slix-or-frame)
        task (str task-name)
        args (seq (map str task-args))
        ;;
        pmap (project-name-to-map proj-name)
        opwd (str (get-project-path pmap))
        ltcl (.getContextClassLoader (Thread/currentThread))
        ;;
        doc (.getDocument out-txtpn)
        eof (fn [d] (max 0 (dec (.getLength doc))))
        ins (fn this
              ([s]
                 (this s nil))
              ([s a]
                 (.insertString doc (eof doc) s a)
                 (.setCaretPosition out-txtpn (eof doc))))
        ;;
        tocntxt (when (is-a-jutest-task? task) (create-test-output-context slix ins *attr-wrn*))
        ant-pos (PipedOutputStream.)
        ant-prs (PrintStream. ant-pos true)
        ant-bfr (BufferedReader. (InputStreamReader. (PipedInputStream. ant-pos)))
        [lein-baos lein-oprs] (get-out-ps)
        test-summary (when (is-a-jutest-task? task) (atom {}))]
    ;; Now carete a lein agent task.
    (fn [_]
      ;; Print a start of task msg and start an ant output msg printer.
      (invoke-later slix #(ins (str "=== " proj-name ": " task " ===\n") *attr-hdr*))
      (future (loop [t tocntxt
                     l (.readLine ant-bfr)]
                (if l
                  (if t
                    (recur (handle-test-output l t) (.readLine ant-bfr))
                    (do
                      (invoke-later slix #(ins (str l "\n")))
                      (recur t (.readLine ant-bfr))))
                  (do
                    #_(lg "eof on ant-bfr")
                    #_(lg "output:" (when t (:output t)))))
                ))
      ;; Run this lein task.
      (let [ct (Thread/currentThread)
            cl (.getContextClassLoader ct)]
        ;; Inherit the planter's class loader or lein crashes.
        (.setContextClassLoader ct ltcl)
        (binding [clojure.core/*out* (java.io.OutputStreamWriter. lein-oprs)
                  leiningen.core/*original-pwd* opwd
                  leiningen.core/*eval-in-lein* false
                  leiningen.core/*exit* false
                  leiningen.core/*test-summary* test-summary
                  lancet/ant-project (leiningen.core/get-ant-project ant-prs ant-prs)
                  lancet.core/ant-project (leiningen.core/get-ant-project ant-prs ant-prs)]
          (try
            (let [result (apply leiningen.core/-main task args)]
              #_(lg "lein result:" result))
            (catch Exception e
              (log-exception e))))
        ;; Restore the original cl or mysterious ThreadDeath occurs.
        (.setContextClassLoader ct cl))
      ;; Close the ant output pipe, which should end the ant output msg
      ;; printer started above. Then print out the lein msg.
      (.close ant-pos)
      ;; Print test summary, if any.
      (when test-summary
        (let [ts @test-summary
              s1 (str "\nRan " (:test ts) " tests containing "
                      (+ (:pass ts) (:fail ts) (:error ts)) " assertions.\n")
              s2 (str (:fail ts) " failures, " (:error ts) " errors.\n")
              at (if (or (pos? (:fail ts)) (pos? (:error ts))) *attr-wrn* *attr-ok*)]
          (invoke-later slix #(ins s1))
          (invoke-later slix #(ins s2 at))
          #_(lg "test summary:" ts)))
      (let [lms (.toString lein-baos)]
        (invoke-later slix #(ins (str lms "#\n\n"))))
      ;; This lein task is finished. Return false to signify the lein agent
      ;; is NOT busy.
      false)))

(defn send-task-to-lein-agent
  [slix-or-frame task]
  (lein-agent-busy slix-or-frame)
  (send (get-lein-agent slix-or-frame) task))

(defn do-lein
  [frame out-txtpn proj-name cmd]
  (let [task (create-lein-task frame out-txtpn proj-name cmd)]
    (send-task-to-lein-agent frame task)))
