(ns slix.planter.controller
  (:use [sevenri config log slix utils]
        [slix.planter core defs io tohandler])
  (:import (java.awt Color FileDialog)
           (java.io PipedInputStream PipedOutputStream PrintStream)
           (java.io BufferedReader File FilenameFilter InputStreamReader)
           (java.net URL URLClassLoader)
           (javax.swing JOptionPane)
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

(defn reserve-lein-agent
  [slix-or-frame new-stat]
  (send (get-lein-agent slix-or-frame)
        (fn [old-stat]
          (or old-stat new-stat))))

(defn send-task-to-lein-agent
  [slix-or-frame tid task]
  (reserve-lein-agent slix-or-frame tid)
  (send (get-lein-agent slix-or-frame) task))

(defn is-lein-agent-busy?
  [slix-or-frame]
  (not (nil? @(get-lein-agent slix-or-frame))))

(defn restart-lein-agent
  [slix-or-frame]
  (restart-agent (get-lein-agent slix-or-frame) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti show-dialog
  (fn [type-kwd frame message title option] type-kwd))

(defmethod show-dialog :default
  [_ frame message title option]
  (JOptionPane/showMessageDialog frame message title option))

(defmethod show-dialog :input
  [_ frame message title option]
  (JOptionPane/showInputDialog frame message title option))

(defmethod show-dialog :confirm
  [_ frame message title option]
  (JOptionPane/showConfirmDialog frame message title option))

(defn print-line
  ([slix txtpn line]
     (print-line slix txtpn line nil))
  ([slix txtpn line attr]
     (let [doc (.getDocument txtpn)
           eof (fn [d] (max 0 (dec (.getLength doc))))
           ins (fn [s a]
                 (.insertString doc (eof doc) s a)
                 (.setCaretPosition txtpn (eof doc)))]
       (invoke-later slix #(ins line attr)))))

(defn form-header-line
  [title]
  (str "=== " title " ===\n"))

(defn print-start-task
  [slix txtpn title]
  (print-line slix txtpn (form-header-line title) *attr-hdr*))

(defn print-end-task
  [slix txtpn]
  (print-line slix txtpn "#\n\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-a-jutest-task?
  [task]
  (if (re-matches #"^jutest.*" task)
    true
    false))

;;;;

(defn create-lein-task
  [slix-or-frame tid out-txtpn proj-name task-name set-ui-wait & task-args]
  (let [slix (get-slix slix-or-frame)
        task (str task-name)
        args (seq (map str task-args))
        ;;
        pmap (project-name-to-map proj-name)
        opwd (str (if (= task "new")
                    (get-project-parent-path pmap)
                    (get-project-path pmap)))
        ltcl (.getContextClassLoader (Thread/currentThread))
        ;;
        ant-pos (PipedOutputStream.)
        ant-prs (PrintStream. ant-pos true)
        ant-bfr (BufferedReader. (InputStreamReader. (PipedInputStream. ant-pos)))
        [lein-baos lein-oprs] (get-out-ps)
        ;;
        ;; Set up special output context if this is a jutest task.
        tocntxt (when (is-a-jutest-task? task)
                  (let [pline (fn this
                                ([s]
                                   (this s nil))
                                ([s a]
                                   (print-line slix out-txtpn s a)))]
                    (create-test-output-context pline *attr-wrn*)))
        test-summary (when (is-a-jutest-task? task) (atom {}))]
    ;; Now carete a lein agent task.
    (fn [id]
      ;; Perform the task only when this task is reserved on lein-agent.
      (when (= id tid)
        (try
          ;; Disable the UI and show the wait cursor. Then print the start of
          ;; task msg.
          (set-ui-wait slix-or-frame)
          (print-start-task slix out-txtpn (str proj-name ": " task))
          ;; Start an ant msg printer.
          (let [amp (future
                      (try
                        (loop [t tocntxt
                               l (.readLine ant-bfr)]
                          (if l
                            (if t
                              (recur (handle-test-output l t) (.readLine ant-bfr))
                              (do
                                (print-line slix out-txtpn (str l "\n"))
                                (recur t (.readLine ant-bfr))))
                            (do
                              #_(lg "eof on ant-bfr")
                              #_(lg "output:" (when t (:output t))))))
                        (catch Exception e)))]
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
            ;; Close the ant output pipe, which should end the ant msg printer
            ;; started above. Refer the amp future object that should wait
            ;; until the amp finish printing.
            (.close ant-pos)
            @amp
            ;; Print test summary, if any.
            (when (and test-summary @test-summary (map? @test-summary))
              (let [ts @test-summary
                    s1 (str "\nRan "
                            (or (:test ts) -1)
                            " tests containing "
                            (+ (or (:pass ts) 0) (or (:fail ts) 0) (or (:error ts) 0))
                            " assertions.\n")
                    s2 (str (or (:fail ts) -1)
                            " failures, "
                            (or (:error ts) -1)
                            " errors.\n")
                    at (if (or (neg? (or (:fail ts) 0))
                               (neg? (or (:error ts) 0)))
                         *attr-wrn*
                         *attr-ok*)]
                (doto slix
                  (print-line out-txtpn s1)
                  (print-line out-txtpn s2 at))
                #_(lg "test summary:" ts)))
            ;; Print lein msg.
            (print-line slix out-txtpn (.toString lein-baos))
            ;; Print the end of task msg, enable the UI, set the original
            ;; cursor back, and update project name if necessary.
            (print-end-task slix out-txtpn)
            (set-ui-wait slix false (if (= task "new") proj-name nil))
            ;; This lein task is finished. Return nil to signify the lein agent
            ;; is free.
            nil)
          (catch Exception e
            (log-exception e)
            nil))))))

(defn do-lein
  [frame out-txtpn proj-name cmd set-ui-wait]
  (let [tid (gensym)
        task (create-lein-task frame tid out-txtpn proj-name cmd set-ui-wait)]
    (send-task-to-lein-agent frame tid task)))

(defn do-lein-new
  [frame out-txtpn proj-name set-ui-wait]
  (let [pmap (project-name-to-map proj-name)
        pdir (get-project-parent-path pmap)]
    (if (.exists (get-project-path pmap))
      (let [msg (str proj-name " exists already.")
            ttl "Project Exists"]
        (show-dialog :message frame msg ttl JOptionPane/OK_OPTION))
      (if (or (.exists pdir) (and (.mkdirs pdir) (.exists pdir)))
        (let [tid (gensym)
              task (create-lein-task frame tid out-txtpn proj-name 'new set-ui-wait (:name pmap))]
          (send-task-to-lein-agent frame tid task))
        (let [msg (str "Cannot create project directory:\n" pdir)
              ttl "Cannot Create Project Directory"]
          (show-dialog :message frame msg ttl JOptionPane/YES_OPTION))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn can-delete-project
  "Return nil proj-name is deletable or a string explaining a reason of why
   not deletable."
  [prj-name]
  (if (= prj-name (str *slix-planter-project*))
    (str prj-name " is a Sevenri system project\nand cannot be deleted.")
    (let [pnl (count prj-name)]
      (when-first [sub-prj-name (filter #(and (zero? (.indexOf % prj-name))
                                              (< pnl (count %)))
                                        (map str (vals (xref-with *xref-planter-project*))))]
        (str prj-name " project contains " sub-prj-name " project\nwhich is opened currently.")))))

;;;;

(defmulti do-command
  (fn [controls set-ui-wait command]
    (keyword (.replace (.toLowerCase command) "..." ""))))

(defmethod do-command :default
  [_ _ command]
  (log-warning "planter: do-command: not implemented:" command))

(defmethod do-command :edit
  [controls _ _]
  (let [frm (:frame controls)
        prn (.getSelectedItem (:project-names controls))
        dlg (FileDialog. frm "Open Project File" FileDialog/LOAD)
        flt (proxy [FilenameFilter] []
              (accept [dir name]
                (if (re-find #"(.*)\.clj$" name)
                  true
                  false)))]
    (doto dlg
      (.setDirectory (str (get-project-path prn)))
      (.setFilenameFilter flt)
      (.show))
    (when-let [f (.getFile dlg)]
      (let [fp (File. (.getDirectory dlg) (str f))]
        (open-slix-with-args {:file fp} 'ced)))))

(defmethod do-command :clone
  [controls set-ui-wait _]
  (let [frame (:frame controls)
        idmsg "repository URL:"
        idttl "Git Clone URL"
        gcurl (show-dialog :input frame idmsg idttl JOptionPane/PLAIN_MESSAGE)]
    (when-not (empty? gcurl)
      (let [[prj-name git] (.split (last (.split gcurl "/")) "\\.")]
        (if (and (not (empty? prj-name)) (= git "git"))
          (let [prj-dir (File. (get-project-dir) prj-name)]
            (if (.exists prj-dir)
              (let [msg (str prj-name " project exists already.")
                    ttl "Project Exists"]
                (show-dialog :message frame msg ttl JOptionPane/OK_OPTION))
              ;; Create a delete task and send it to lein-agent to run.
              (let [tid (gensym)
                    slx (get-slix frame)
                    txt (:output-text controls)
                    tsk (fn [id]
                          (when (= id tid)
                            (set-ui-wait slx)
                            (print-start-task slx txt (str "Cloning " gcurl))
                            ;;
                            (let [result (do-git-clone (.getParent prj-dir) gcurl)]
                              (print-line slx txt (:out result))
                              (when (not (empty? (:err result)))
                                (print-line slx txt (:err result)*attr-wrn*))
                              ;;
                              (print-end-task slx txt)
                              (set-ui-wait slx false prj-name))))]
                (send-task-to-lein-agent slx tid tsk))))
          ;;
          (let [msg (str "Not a git repository URL?:\n" gcurl)
                ttl "Not A Git Repo URL"]
            (show-dialog :message frame msg ttl JOptionPane/OK_OPTION)))))))

(defmethod do-command :delete
  [controls set-ui-wait _]
  (let [prj-name (.getSelectedItem (:project-names controls))
        undl-rsn (can-delete-project prj-name)
        conf-frm (:frame controls)
        conf-msg (or undl-rsn (str "OK to delete " prj-name "?"))
        conf-ttl (str (if undl-rsn "Cannot Delete" "Deleting") " Project")
        response (show-dialog (if undl-rsn :message :confirm) conf-frm conf-msg conf-ttl
                              (if undl-rsn JOptionPane/YES_OPTION JOptionPane/YES_NO_CANCEL_OPTION))]
    (when (and (nil? undl-rsn) (= response JOptionPane/YES_OPTION))
      (set-ui-wait conf-frm)
      (delete-project prj-name)
      (set-ui-wait conf-frm false :delete))))
        
(defmethod do-command :new
  [controls set-ui-wait _]
  (let [frame (:frame controls)
        idmsg "New Project Name:"
        idttl "New Project"
        prj-name (show-dialog :input frame idmsg idttl JOptionPane/PLAIN_MESSAGE)]
    (when-not (empty? prj-name)
      (let [out-txtpn (:output-text controls)
            safe-prj-name (safesymstr prj-name)]
        (if (= prj-name safe-prj-name)
          (do-lein-new frame out-txtpn safe-prj-name set-ui-wait)
          (let [cdmsg (str prj-name
                           " contains invalid project name character(s)\n"
                           "and this name is used instead:\n" safe-prj-name)
                cdttl "New Project Name"
                rspns (show-dialog :confirm frame cdmsg cdttl JOptionPane/YES_NO_CANCEL_OPTION)]
            (when (= rspns JOptionPane/YES_OPTION)
              (do-lein-new frame out-txtpn safe-prj-name set-ui-wait))))))))
