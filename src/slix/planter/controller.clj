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

(defn create-lein-agent
  [slix-or-frame]
  (if-let [slix (get-slix slix-or-frame)]
    (put-slix-prop slix :lein-agent (agent nil))
    (throw (IllegalArgumentException. "neither slix or frame"))))

(defn get-lein-agent
  [slix-or-frame]
  (if-let [slix (get-slix slix-or-frame)]
    (if-let [la (get-slix-prop slix :lein-agent)]
      la
      (throw (IllegalStateException. "no lein-agent")))
    (throw (IllegalArgumentException. "neither slix or frame"))))

(defn is-lein-agent-busy?
  [slix-or-frame]
  (not (nil? @(get-lein-agent slix-or-frame))))

(defn restart-lein-agent
  [slix-or-frame]
  (restart-agent (get-lein-agent slix-or-frame) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-a-jutest-task?
  [task]
  (if (re-matches #"^jutest.*" task)
    true
    false))

(defn create-lein-task
  [slix-or-frame tid out-txtpn proj-name task-name set-ui-wait & task-args]
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
    (fn [id]
      ;; Perform the task only when the preset task id matches.
      (when (= id tid)
        ;; Disable UI controls and show the wait cursor.
        (set-ui-wait slix-or-frame true)
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
          (let [sw (java.io.OutputStreamWriter. lein-oprs)
                ap (leiningen.core/get-ant-project ant-prs ant-prs)]
            (binding [clojure.core/*out* sw
                      clojure.core/*err* sw
                      lancet/ant-project ap
                      lancet.core/ant-project ap
                      leiningen.core/*original-pwd* opwd
                      leiningen.core/*eval-in-lein* false
                      leiningen.core/*exit* false
                      leiningen.core/*test-summary* test-summary]
              (try
                (let [result (apply leiningen.core/-main task args)]
                  #_(lg "lein result:" result))
                (catch Exception e
                  (log-exception e))))))
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
        ;; This lein task is finished. Return nil to signify the lein agent
        ;; is NOT busy after enabling UI controls.
        (set-ui-wait slix-or-frame false)
        nil))))

(defn send-task-to-lein-agent
  [slix-or-frame tid task]
  (let [la (get-lein-agent slix-or-frame)]
    (doto la
      (send (fn [id] (or id tid)))
      (send task))))

(defn do-lein
  [frame out-txtpn proj-name cmd set-ui-wait]
  (let [tid (gensym)
        task (create-lein-task frame tid out-txtpn proj-name cmd set-ui-wait)]
    (send-task-to-lein-agent frame tid task)))
