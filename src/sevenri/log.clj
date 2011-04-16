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

(ns sevenri.log
  (:require [clojure stacktrace])
  (:use [sevenri config defs refs utils])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream
                    File PrintStream PrintWriter StringWriter)
           (java.text SimpleDateFormat)
           (java.util Date)
           (java.util.logging Level Logger LogManager)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sevenri-logger-name
  []
  *sevenri-logger-name*)

(defn get-sevenri-logger
  []
  *sevenri-logger*)

(defn get-sevenri-log-file
  []
  *sevenri-log-file*)

(defn get-std-in
  []
  *standard-in*)

(defn get-std-out
  []
  *standard-out*)

(defn get-std-err
  []
  *standard-err*)

(defn get-sid-log-dir
  []
  (get-sid-dir (get-default :sid :log :dir-name)))

(defn get-last-exception
  []
  @*e*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-msgs
  [& msgs]
  (apply println *sevenri-logger-header* msgs))

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
  (get-dir (get-sid-dir (get-default :sid :sevenri :dir-name)) (get-default :sid :sevenri :exception :dir-name)))

(defn get-exception-listeners-file
  []
  (File. (get-sid-sevenri-exception-dir) (str (get-default :sid :sevenri :exception :listeners))))

(defn load-exception-listeners
  []
  (let [elfile (get-exception-listeners-file)]
    (when (.exists elfile)
      (try
        (let [elisteners (read-string (slurp elfile))]
          (reset! *exception-listeners-cache* elisteners))))))

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
  [#^Exception e ens]
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

(defmacro lg
  "alias of log-info"
  [& msgs]
  `(log-info ~@msgs))

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

(defn log-exception
  ([#^Exception e]
     (log-exception e nil))
  ([#^Exception e ns]
     (when-not (instance? ThreadDeath e)
       (let [sw (StringWriter.)
             pw (PrintWriter. sw)
             ts (str "log-exception" (when ns (str " (" ns ")")) ":")]
         (binding [*out* pw]
           (clojure.stacktrace/print-stack-trace e))
         (if *sevenri-logger*
           (.logp *sevenri-logger* Level/SEVERE *sevenri-logger-name* ts (.toString sw))
           (println *sevenri-logger-header* ts (.toString sw)))))
     ;;
     (reset! *e* e)
     (future (dispatch-exception e ns))))

(defn log-uncaught-exception
  [#^Thread t #^Exception e]
  (when-not (instance? ThreadDeath e)
    (let [sw (StringWriter.)
          pw (PrintWriter. sw)]
      (binding [*out* pw]
        (clojure.stacktrace/print-stack-trace e))
      (if *sevenri-logger*
        (.logp *sevenri-logger* Level/SEVERE *sevenri-logger-name* "log-uncaught-exception"
               (str "Uncaught exception in thread: " (.toString t) "\n" (.toString sw)))
        (println *sevenri-logger-header*
                 "log-exception: uncaught exception in thread:" (.toString t) "\n" (.toString sw))))
    ;;
    (reset! *e* e)
    (future (dispatch-exception e nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -get-sid-log-dir?
  []
  (if (.isDirectory (get-sid-log-dir)) true false))

(defn- -cleanup-sid-log-dir?
  []
  (loop [lfs (find-files '.lck (get-sid-log-dir))]
    (when (and (seq lfs) (.canWrite (first lfs)))
      (.delete (first lfs))
      (recur (rest lfs))))
  true)

(defn get-new-log-file
  ([]
     (let [lfs (find-files '.log (get-sid-log-dir))
           lfc (get-default :sid :log :file-count)]
       (if (< (count lfs) lfc)
         (get-new-log-file lfs)
         (loop [lfs lfs]
           (.delete (first lfs))
           (if (< (count (rest lfs)) lfc)
             (get-new-log-file (rest lfs))
             (recur (rest lfs)))))))
  ([_]
     (let [pfx (.format (SimpleDateFormat. "yyMMdd-HHmmss") (Date.))
           lfn (File. (get-sid-log-dir) (str (get-default :sid :log :file-body-name) \- pfx ".log"))]
       lfn)))

(defn- -create-logging-properties
  []
  (let [lf (get-new-log-file)]
    (reset-sevenri-log-file lf)
    (str (println-str "java.util.logging.FileHandler.pattern =" (.getCanonicalPath lf)))))

(defn get-logging-properties-inputstream
  [cfgs]
  (let [rld (get-resources-dir (get-default :src :resources :logger :dir-name))
        lcf (File. rld (str (get-default :src :resources :logger :configuration-file)))]
    (ByteArrayInputStream. (.getBytes (str (slurp lcf :encoding "UTF-8") \newline cfgs)))))

(defn- -get-logger?
  []
  (if *sevenri-logger*
    true
    (let [cfgs (-create-logging-properties)
          logger-name (str (get-default :sid :log :logger-name))
          logger-header (get-default :sid :log :logger-header)]
      (with-open [is (get-logging-properties-inputstream cfgs)]
        (.readConfiguration (LogManager/getLogManager) is)
        (reset-sevenri-logger logger-name logger-header (Logger/getLogger logger-name))
        (when-not *sevenri-logger*
          (throw (RuntimeException. "get a logger failed")))
        (reset-sevenri-logger-popup Level/WARNING)
        true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defmacro starting-up
  [& preds]
  `(with-making-dir
     (every? true? (map #(do
                           (print-info "starting up:" (:name (meta %)))
                           (%))
                        (list ~@preds)))))

(defmacro shutting-down
  [& preds]
  `(every? true? (map #(do
                         (print-info "shutting down:" (:name (meta %)))
                         (%))
                      (list ~@preds))))

;;;;

(defn -save-std-inouterr?
  []
  (reset-standard-in-out-err *in* *out* *err*)
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
  (when-not *thread-default-uncaught-exception-handler*
    (reset-thread-default-uncaught-exception-handler (proxy [Thread$UncaughtExceptionHandler] []
                                                       (uncaughtException [t e]
                                                                          (log-uncaught-exception t e)))))
  (Thread/setDefaultUncaughtExceptionHandler *thread-default-uncaught-exception-handler*)
  true)

(defn- -get-sid-sevenri-exception-dir?
  []
  (if (.isDirectory (get-sid-sevenri-exception-dir)) true false))

(defn- -load-exception-listeners?
  []
  (load-exception-listeners)
  true)

;;;;

(defn startup-log?
  []
  (starting-up
   -save-std-inouterr?
   -get-sid-log-dir?
   -cleanup-sid-log-dir?
   -get-logger?
   -redirect-system-out-and-err?
   -install-thread-default-uncaught-exception-handler?
   -get-sid-sevenri-exception-dir?
   -load-exception-listeners?))

(defn shutdown-log?
  []
  true)
