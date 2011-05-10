;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Sevenri, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Sevenri logging lib"}
  sevenri.log
  (:require [clojure.stacktrace :as cst])
  (:use [sevenri config defs refs])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream
                    File PrintStream PrintWriter StringWriter)
           (java.text SimpleDateFormat)
           (java.util Date)
           (java.util.logging Level Logger LogManager)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sevenri-logger
  []
  *sevenri-logger*)

(defn get-sevenri-logger-name
  []
  *sevenri-logger-name*)

(defn get-sevenri-logger-popup-info
  []
  {:level *sevenri-logger-popup-level* :sec *sevenri-logger-popup-sec*})

(defn get-sevenri-log-file
  []
  *sevenri-log-file*)

(defn get-sid-log-dir
  []
  (let [dir (File. *sid-path* (str (get-config 'sid.log.dir)))]
    (when-not (.exists dir)
      (when-not (.mkdir dir)
        (throw (RuntimeException. "get-sid-log-dir: mkdir failed"))))
    dir))

;;;;

(defn get-std-in
  []
  *standard-in*)

(defn get-std-out
  []
  *standard-out*)

(defn get-std-err
  []
  *standard-err*)

;;;;

(defn get-last-exception
  []
  @*e*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-msgs
  [& msgs]
  (apply println "sevenri:" msgs))

(defn print-info
  [& msgs]
  (apply print-msgs "info:" msgs))

(defn print-warning
  [& msgs]
  (apply print-msgs "warning:" msgs))

(defn print-severe
  [& msgs]
  (apply print-msgs "severe:" msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These fns are resolved at the startup time.
(using-fns log slix
           [get-all-slix-sn get-slix-ns reload-sn?])

(defn get-exception-listeners
  []
  @*exception-listeners-cache*)

(defn get-sid-sevenri-exception-dir
  []
  (let [sevenri-dir (File. *sid-path* (str (get-config 'sid.sevenri.dir)))
        exception-dir (File. sevenri-dir (str (get-config 'sid.sevenri.exception.dir)))]
    (when-not (.exists sevenri-dir)
      (when-not (.mkdir sevenri-dir)
        (throw (RuntimeException. "get-sid-sevenri-exception-dir failed (sevenri-dir)"))))
    (when-not (.exists exception-dir)
      (when-not (.mkdir exception-dir)
        (throw (RuntimeException. "get-sid-sevenri-exception-dir failed (exception-dir)"))))
    exception-dir))

(defn get-exception-listeners-file
  []
  (File. (get-sid-sevenri-exception-dir) (get-config 'sid.sevenri.exception.listeners-file-name)))

(defn load-exception-listeners
  []
  (let [elfile (get-exception-listeners-file)]
    (when (and (.exists elfile) (pos? (.length elfile)))
      (try
        (let [elisteners (read-string (slurp elfile))]
          (reset! *exception-listeners-cache* elisteners))
        (catch Exception e
          (throw (RuntimeException. (str "load-exception-listeners failed:" e))))))))

(defn register-exception-listener
  [listener-sn listener-name]
  (when (and (symbol? listener-sn)
             (symbol? listener-name))
    (let [elisteners (assoc @*exception-listeners-cache*
                       (symbol (str listener-sn \/ listener-name))
                       [listener-sn listener-name])
          elfile (get-exception-listeners-file)]
      (reset! *exception-listeners-cache* elisteners)
      (spit elfile elisteners))))

(defn unregister-exception-listener
  [listener-sn listener-name]
  (when (and (symbol? listener-sn)
             (symbol? listener-name))
    (let [elisteners (dissoc @*exception-listeners-cache*
                             (symbol (str listener-sn \/ listener-name)))
          elfile (get-exception-listeners-file)]
      (reset! *exception-listeners-cache* elisteners)
      (spit elfile elisteners))))
  
(defn dispatch-exception
  [^Exception e ens]
  (doseq [elisteners (get-exception-listeners)]
    (let [[_ [sn name]] elisteners
          ns (log-using-get-slix-ns-slix sn)]
      (try
        (if (and (get (log-using-get-all-slix-sn-slix) sn)
                 (log-using-reload-sn?-slix sn))
          (if-let [hdlr (ns-resolve ns name)]
            (when (fn? (var-get hdlr))
              (hdlr e ens))
            (unregister-exception-listener sn name))
          (unregister-exception-listener sn name))
        (catch Exception ex
          (unregister-exception-listener sn name)
          (declare log-exception)
          (log-exception ex ns))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These fns are resolved at the startup time.
(using-fns log ui
           [popup-log-notifier])

(defn get-sevenri-logger-popup-level
  []
  (cond
   (instance? java.util.logging.Level *sevenri-logger-popup-level*)
     (.intValue *sevenri-logger-popup-level*)
   (instance? Integer *sevenri-logger-popup-level*)
     *sevenri-logger-popup-level*
   :else
     (.intValue java.util.logging.Level/OFF)))

(defn log-msgs
  [lvl & msgs]
  (.logp *sevenri-logger* lvl *sevenri-logger-name* "log-msgs" (apply print-str msgs))
  (when (<= (get-sevenri-logger-popup-level) (.intValue lvl))
    (log-using-popup-log-notifier-ui lvl *sevenri-logger-popup-sec*)))

(defn log-info
  [& msgs]
  (apply log-msgs Level/INFO msgs))

(defn log-warning
  [& msgs]
  (apply log-msgs Level/WARNING msgs))

(defn log-severe
  [& msgs]
  (apply log-msgs Level/SEVERE msgs))

(defmacro lg
  "alias of log-info"
  [& msgs]
  `(log-info ~@msgs))

;;;;

(defn get-stack-trace-print-lines
  [^Exception e]
  (let [sw (StringWriter.)
        pw (PrintWriter. sw)]
    (binding [*out* pw]
      (cst/print-stack-trace e))
    (.toString sw)))

(defn log-exception
  ([^Exception e]
     (log-exception e nil))
  ([^Exception e ns]
     (when-not (instance? ThreadDeath e)
       (let [lemsg (str "log-exception" (when ns (str " (" ns ")")) ":")
             stpls (get-stack-trace-print-lines e)]
         (if *sevenri-logger*
           (.logp *sevenri-logger* Level/SEVERE *sevenri-logger-name* lemsg stpls)
           (println "sevenri:" lemsg stpls))))
     ;;
     (reset! *e* e)
     (future (dispatch-exception e ns))))

(defn log-uncaught-exception
  [^Thread t ^Exception e]
  (when-not (instance? ThreadDeath e)
    (let [tname (.toString t)
          stpls (get-stack-trace-print-lines e)]
      (if *sevenri-logger*
        (.logp *sevenri-logger* Level/SEVERE *sevenri-logger-name* "log-uncaught-exception"
               (str "Uncaught exception in thread: " tname "\n" stpls))
        (println "sevenri: log-exception: uncaught exception in thread:" tname "\n" stpls)))
    ;;
    (reset! *e* e)
    (future (dispatch-exception e nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

;;;; startup

(defn- -get-sid-log-dir?
  []
  (let [dir (get-sid-log-dir)]
    (and (.isDirectory dir) (.canWrite dir))))

(defn- -get-sid-log-files
  [file-spec-regex]
  (.listFiles (get-sid-log-dir)
              (proxy [java.io.FilenameFilter] []
                (accept [dir name]
                  (if (and (= dir (get-sid-log-dir))
                           (re-matches file-spec-regex name))
                    true
                    false)))))

(defn- -create-new-log-file
  []
  (let [pfx (.format (SimpleDateFormat. "yyMMdd-HHmmss") (Date.))
        lfn (File. (get-sid-log-dir) (str (get-config 'sid.log.file-name) \- pfx '.log))]
    lfn))

(defn- -delete-oldest-log-file
  []
  (let [lfs (-get-sid-log-files #".*\.log$")]
    (when (< (get-config 'sid.log.file-count) (count lfs))
      (.delete (first (sort #(neg? (.compareTo (str %1) (str %2))) lfs))))))

(defn- -create-logging-properties
  []
  (let [lf (-create-new-log-file)]
    (redef! *sevenri-log-file* lf)
    (str (println-str "java.util.logging.FileHandler.pattern =" (.getCanonicalPath lf)))))

(defn- -get-logging-properties-inputstream
  [cfgs]
  (let [ldr (reduce (fn [d p] (File. d (str (get-config p))))
                    (get-user-dir)
                    ['src.dir
                     'src.resources.dir
                     'src.resources.logger.dir])
        lcf (File. ldr (get-config 'src.resources.logger.config-file-name))]
    (when-not (.exists lcf)
      (throw (RuntimeException. "-get-logging-properties-inputstream failed")))
    (ByteArrayInputStream. (.getBytes (str (slurp lcf :encoding "UTF-8") \newline cfgs)))))

(defn- -get-logger?
  []
  (let [cfgs (-create-logging-properties)
        logger-name (get-config 'sid.log.logger-name)]
    (with-open [is (-get-logging-properties-inputstream cfgs)]
      (.readConfiguration (LogManager/getLogManager) is)
      (when-not (Logger/getLogger logger-name)
        (throw (RuntimeException. "-get-logger? failed"))))
    ;;
    (redef! *sevenri-logger* (Logger/getLogger logger-name))
    (redef! *sevenri-logger-name* logger-name)
    (redef! *sevenri-logger-popup-level* (get-config 'sid.log.popup.level))
    (redef! *sevenri-logger-popup-sec* (get-config 'sid.log.popup.sec))
    ;;
    true))

(defn -save-std-inouterr?
  []
  (redef! *standard-in*  *in*)
  (redef! *standard-out* *out*)
  (redef! *standard-err* *err*)
  true)

(defn- -redirect-system-out-and-err?
  []
  (when *sevenri-logger*
    (let [csf (fn [msg]
                (proxy [ByteArrayOutputStream] []
                  (flush []
                         (locking this
                           (proxy-super flush)
                           (let [s (.toString this)]
                             (proxy-super reset)
                             (when (and (pos? (count s))
                                        (not= s (System/getProperty "line.separator")))
                               (log-info msg s)))))))
          lpo (PrintStream. (csf "SysOut:") true)
          lpe (PrintStream. (csf "SysErr:") true)]
      (System/setOut lpo)
      (System/setErr lpe)))
  true)

(defn get-thread-default-uncaught-exception-handler
  []
  *thread-default-uncaught-exception-handler*)

(defn- -install-thread-default-uncaught-exception-handler?
  []
  (let [handler (proxy [Thread$UncaughtExceptionHandler] []
                  (uncaughtException [t e]
                    (log-uncaught-exception t e)))]
    (redef! *thread-default-uncaught-exception-handler* handler)
    (Thread/setDefaultUncaughtExceptionHandler handler)
    true))

(defn- -load-exception-listeners?
  []
  (if (.isDirectory (get-sid-sevenri-exception-dir))
    (do
      (load-exception-listeners)
      true)
    false))

;;;; shutdown

(defn- -cleanup-sid-log-dir?
  []
  (doseq [lf (-get-sid-log-files #".*\.lck$")]
    (when (.canWrite lf)
      (.delete lf)))
  true)

(defn- -delete-oldest-log-file?
  []
  (-delete-oldest-log-file)
  true)

;;;;

(defn startup-log?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -get-sid-log-dir?
          -get-logger?
          -save-std-inouterr?
          -redirect-system-out-and-err?
          -install-thread-default-uncaught-exception-handler?
          -load-exception-listeners?)))

(defn shutdown-log?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -cleanup-sid-log-dir?
          -delete-oldest-log-file?)))
