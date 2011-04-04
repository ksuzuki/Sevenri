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

(def *attr-std* (let [a (SimpleAttributeSet.)]
                  (StyleConstants/setForeground a Color/black)
                  a))

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
  (intern 'leiningen.core '*help-excluding-tasks* nil)
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
  (if (not (lein-loaded?))
    false
    (not (nil? @(get-lein-agent slix-or-frame)))))

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

(defn create-piped-streams-and-reader
  ([]
     (create-piped-streams-and-reader false))
  ([writer?]
     (let [pos (PipedOutputStream.)
           prs (PrintStream. pos true)
           rdr (BufferedReader. (InputStreamReader. (PipedInputStream. pos)))]
       [pos (if writer? (java.io.OutputStreamWriter. prs) prs) rdr])))

(defn msg-printer
  ([slix txtpn reader attr]
     (msg-printer slix txtpn reader attr nil))
  ([slix txtpn reader attr test-outctxt]
     (future (try
               (loop [t test-outctxt
                      l (.readLine reader)]
                 (when l
                   (if t
                     (recur (handle-test-output l t) (.readLine reader))
                     (do
                       (print-line slix txtpn (str l "\n") attr)
                       (recur t (.readLine reader))))))
               (catch Exception e)))))

(defn print-test-summary
  [slix txtpn test-summary]
  #_(lg "planter: print-test-summary:" test-summary)
  (let [ts test-summary
        s1 (str "\nRan "
                (or (:test ts) -1)
                " tests containing "
                (+ (or (:pass ts) 0) (or (:fail ts) 0) (or (:error ts) 0))
                " assertions.\n")
        s2 (str (or (:fail ts) -1)
                " failures, "
                (or (:error ts) -1)
                " errors.\n")
        at (if (or (not (zero? (or (:fail ts) -1)))
                   (not (zero? (or (:error ts) -1))))
             *attr-wrn*
             *attr-ok*)]
    (doto slix
      (print-line txtpn s1)
      (print-line txtpn s2 at))))

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
        ldjs (when (and (not= task "new")
                        (.exists (File. (get-project-path pmap) "lib/dev")))
               (for [f (file-seq (File. (get-project-path pmap) "lib/dev"))
                     :when (re-matches #".*\.jar$" (.getName f))]
                 (.toURL f)))
        ;;
        [ant-oso ant-pso ant-rdo] (create-piped-streams-and-reader)
        [ant-ose ant-pse ant-rde] (create-piped-streams-and-reader)
        ;;
        [lin-oso lin-pso lin-rdo] (create-piped-streams-and-reader true)
        [lin-ose lin-pse lin-rde] (create-piped-streams-and-reader true)
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
    ;;
    ;; Now carete a lein agent task fn.
    ;;
    (fn [id]
      ;; Perform the task only when this task is reserved on lein-agent.
      (when (= id tid)
        (try
          ;; Disable the UI and show the wait cursor. Then print the start of
          ;; task msg.
          (set-ui-wait slix-or-frame)
          (print-start-task slix out-txtpn (str proj-name ": " task))
          ;; Start ant and lein msg printers.
          (let [ampo (msg-printer slix out-txtpn ant-rdo *attr-std* tocntxt)
                ampe (msg-printer slix out-txtpn ant-rde *attr-wrn* tocntxt)
                lmpo (msg-printer slix out-txtpn lin-rdo *attr-std*)
                lmpe (msg-printer slix out-txtpn lin-rde *attr-wrn*)]
            ;; Run this lein task.
            (let [ct (Thread/currentThread)
                  cl (.getContextClassLoader ct)]
              ;; Inherit the planter's class loader or lein crashes.
              (.setContextClassLoader ct (if (seq ldjs)
                                           (URLClassLoader. (into-array ldjs) ltcl)
                                           ltcl))
              (let [ap (leiningen.core/get-ant-project ant-pso ant-pse)]
                (binding [clojure.core/*out* lin-pso
                          clojure.core/*err* lin-pse
                          lancet/ant-project ap
                          lancet.core/ant-project ap
                          leiningen.core/*original-pwd* opwd
                          leiningen.core/*eval-in-lein* false
                          leiningen.core/*exit* false
                          leiningen.core/*test-summary* test-summary
                          leiningen.core/*help-excluding-tasks* *excluding-tasks*]
                  (try
                    (let [result (apply leiningen.core/-main task args)]
                      #_(lg "lein result:" result))
                    (catch Exception e
                      #_(log-exception e))))))
            ;; Print test summary, if any.
            (when (and test-summary @test-summary (map? @test-summary))
              (print-test-summary slix out-txtpn @test-summary))
            ;; Close the ant and lein output pipes, which should end their
            ;; msg printers started above. Refer the future objects that
            ;; should wait until they finish printing.
            (do (.close ant-pso) @ampo (.close ant-pse) @ampe)
            (do (.close lin-pso) @lmpo (.close lin-pse) @lmpo)
            ;; Special setup for a new slix project.
            (when (and (= task "new") (project-exists? pmap))
              (when-not (setup-slix-project? pmap)
                (print-line slix out-txtpn "Setup for slix project failed" *attr-wrn*)))
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

(defn project-exists-ui?
  [frame prj-name]
  (let [safe-prj-name (safesymstr prj-name)
        can-continue? (if (= prj-name safe-prj-name)
                        true
                        (let [cdmsg (str prj-name
                                         " contains invalid project name character(s)\n"
                                         "and this name is used instead:\n" safe-prj-name)
                              cdttl "Safe Project Name"
                              rspns (show-dialog :confirm frame cdmsg cdttl JOptionPane/YES_NO_CANCEL_OPTION)]
                          (= rspns JOptionPane/YES_OPTION)))]
    (if can-continue?
      (let [pns (filter #(= safe-prj-name %) (map str (keys (get-project-name-config-map))))]
        (if (seq pns)
          (let [msg (str safe-prj-name " exists already.")
                ttl "Project Exists"]
            (show-dialog :message frame msg ttl JOptionPane/YES_OPTION)
            true)
          false))
      ;; Pretend as though it exists.
      true)))

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
        (if (and (= git "git")
                 (not (empty? prj-name))
                 (not (project-exists-ui? prj-name)))
          (let [safe-pn (safesymstr prj-name)
                prj-dir (File. (get-project-dir) safe-pn)
                ;; Create a clone task.
                tid (gensym)
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
                          (set-ui-wait slx false safe-pn))))]
            (send-task-to-lein-agent slx tid tsk))
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
      (set-ui-wait conf-frm false :refresh))))

(defmethod do-command :move
  [controls set-ui-wait _]
  (let [oldpn (.getSelectedItem (:project-names controls))
        frame (:frame controls)
        idmsg "Destination Project Name:"
        idttl "Moving Project"
        newpn (show-dialog :input frame idmsg idttl JOptionPane/PLAIN_MESSAGE)]
    (when (and (not (empty? newpn))
               (not (project-exists-ui? frame newpn)))
      (let [safe-npn (safesymstr newpn)
            src-pdir (get-project-path oldpn)
            dst-pdir (get-project-path safe-npn)
            renamed? (atom false)]
        (set-ui-wait frame)
        @(future (try (reset! renamed? (.renameTo src-pdir dst-pdir))))
        (set-ui-wait frame false safe-npn)
        (when-not renamed?
          (let [errmsg (str "Moving " oldpn " to " safe-npn " failed.")
                errttl "Moving Project Failed"]
            (show-dialog :message frame errmsg errttl JOptionPane/YES_OPTION)))))))
        
(defmethod do-command :new
  [controls set-ui-wait _]
  (let [frame (:frame controls)
        idmsg "New Project Name:"
        idttl "Creating New Project"
        prj-name (show-dialog :input frame idmsg idttl JOptionPane/PLAIN_MESSAGE)]
    (when (and (not (empty? prj-name))
               (not (project-exists-ui? frame prj-name)))
      (do-lein-new frame (:output-text controls) (safesymstr prj-name) set-ui-wait))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-build-project-and-run
  [slix controls set-ui-wait bprm init-close?]
  (let [{:keys [slix-name name args]} bprm
        proj-name (get-slix-fqns slix-name)]
    #_(lg "build project:" proj-name "and run:" slix-name "with name:" name " and args:" args)
    (future
      (let [frm (slix-frame slix)]
        (if-not (project-exists? proj-name)
          (let [msg (str proj-name " project doesnot exist.")
                ttl "Project Not Exist"]
            (JOptionPane/showMessageDialog frm msg ttl JOptionPane/ERROR_MESSAGE))
          ;;
          (do
            (when init-close?
              (invoke-later slix #(.setSelectedItem (:project-names controls) (str proj-name))))
            (invoke-later slix #(.doClick (:jar controls)))
            ;;
            (Thread/sleep 500)
            (loop [busy? (is-lein-agent-busy? slix)]
              (when busy? (recur (is-lein-agent-busy? slix))))
            ;;
            (if-not (is-project-built? proj-name)
              (let [msg (str "Building " proj-name " project failed.")
                    ttl "Build Failed"]
                (JOptionPane/showMessageDialog frm msg ttl JOptionPane/ERROR_MESSAGE))
              ;;
              (do
                (if args
                  (if name
                    (open-slix-with-args args slix-name name)
                    (open-slix-with-args args slix-name))
                  (if name
                    (open-slix slix-name name)
                    (open-slix slix-name)))
                (when init-close?
                  (close-slix slix))))))))))

(defn add-do-build-project-and-run-to-xref
  [slix controls set-ui-wait]
  (add-to-xref slix *build-project-and-run*
               (fn [bprm]
                 (do-build-project-and-run slix controls set-ui-wait bprm false))))
