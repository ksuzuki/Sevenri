;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Openar, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns openar.log
  (:require [clojure stacktrace])
  (:use [openar config defs refs utils])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream
                    File PrintStream PrintWriter StringWriter)
           (java.text SimpleDateFormat)
           (java.util Date)
           (java.util.logging Level Logger LogManager)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-openar-logger-name
  []
  *openar-logger-name*)

(defn get-openar-logger
  []
  *openar-logger*)

(defn get-openar-log-file
  []
  *openar-log-file*)

(defn get-std-in
  []
  *standard-in*)

(defn get-std-out
  []
  *standard-out*)

(defn get-std-err
  []
  *standard-err*)

(defn get-dop-log-dir
  []
  (get-dop-dir (get-default :dop :log :dir-name)))

(defn get-last-exception
  []
  @*e*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-msgs
  [& msgs]
  (apply println *openar-logger-header* msgs))

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
(using-fns log oplix
           [get-all-oplix-on get-oplix-fqns reload-on?])

(defn get-exception-listeners
  []
  @*exception-listeners-cache*)

(defn get-dop-openar-exception-dir
  []
  (get-dir (get-dop-dir (get-default :dop :openar :dir-name)) (get-default :dop :openar :exception :dir-name)))

(defn get-exception-listeners-file
  []
  (File. (get-dop-openar-exception-dir) (str (get-default :dop :openar :exception :listeners))))

(defn load-exception-listeners
  []
  (let [elfile (get-exception-listeners-file)]
    (when (.exists elfile)
      (try
        (let [elisteners (read-string (slurp elfile))]
          (reset! *exception-listeners-cache* elisteners))))))

(defn register-exception-listener
  [listener-on listener-name]
  (when (and (symbol? listener-on)
             (symbol? listener-name))
    (let [elisteners (assoc @*exception-listeners-cache*
                       (symbol (str listener-on \/ listener-name))
                       [listener-on listener-name])
          elfile (get-exception-listeners-file)]
      (reset! *exception-listeners-cache* elisteners)
      (spit elfile elisteners))))

(defn unregister-exception-listener
  [listener-on listener-name]
  (when (and (symbol? listener-on)
             (symbol? listener-name))
    (let [elisteners (dissoc @*exception-listeners-cache*
                             (symbol (str listener-on \/ listener-name)))
          elfile (get-exception-listeners-file)]
      (reset! *exception-listeners-cache* elisteners)
      (spit elfile elisteners))))
  
(defn dispatch-exception
  [#^Exception e efqon]
  (doseq [elisteners (get-exception-listeners)]
    (let [[_ [on name]] elisteners
          fqon (log-using-get-oplix-fqns-oplix on)]
      (try
        (if (and (get (log-using-get-all-oplix-on-oplix) on)
                 (log-using-reload-on?-oplix on))
          (if-let [hdlr (ns-resolve fqon name)]
            (when (fn? (var-get hdlr))
              (hdlr e efqon))
            (unregister-exception-listener on name))
          (unregister-exception-listener on name))
        (catch Exception ex
          (unregister-exception-listener on name)
          (declare log-exception)
          (log-exception ex fqon))))))

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
  (.logp *openar-logger* lvl *openar-logger-name* "log-msgs" (apply print-str msgs))
  (when (<= (get-openar-logger-popup-level) (.intValue lvl))
    (log-using-popup-log-notifier-ui lvl *openar-logger-popup-sec*)))

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
  ([#^Exception e fqon]
     (when-not (instance? ThreadDeath e)
       (let [sw (StringWriter.)
             pw (PrintWriter. sw)
             ts (str "log-exception" (when fqon (str " (" fqon ")")) ":")]
         (binding [*out* pw]
           (clojure.stacktrace/print-stack-trace e))
         (if *openar-logger*
           (.logp *openar-logger* Level/SEVERE *openar-logger-name* ts (.toString sw))
           (println *openar-logger-header* ts (.toString sw)))))
     ;;
     (reset! *e* e)
     (future (dispatch-exception e fqon))))

(defn log-uncaught-exception
  [#^Thread t #^Exception e]
  (when-not (instance? ThreadDeath e)
    (let [sw (StringWriter.)
          pw (PrintWriter. sw)]
      (binding [*out* pw]
        (clojure.stacktrace/print-stack-trace e))
      (if *openar-logger*
        (.logp *openar-logger* Level/SEVERE *openar-logger-name* "log-uncaught-exception"
               (str "Uncaught exception in thread: " (.toString t) "\n" (.toString sw)))
        (println *openar-logger-header*
                 "log-exception: uncaught exception in thread:" (.toString t) "\n" (.toString sw))))
    ;;
    (reset! *e* e)
    (future (dispatch-exception e nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-dop-log-dir?
  []
  (if (.isDirectory (get-dop-log-dir)) true false))

(defn cleanup-dop-log-dir?
  []
  (loop [lfs (find-files '.lck (get-dop-log-dir))]
    (when (and (seq lfs) (.canWrite (first lfs)))
      (.delete (first lfs))
      (recur (rest lfs))))
  true)

(defn get-new-log-file
  ([]
     (let [lfs (find-files '.log (get-dop-log-dir))
           lfc (get-default :dop :log :file-count)]
       (if (< (count lfs) lfc)
         (get-new-log-file lfs)
         (loop [lfs lfs]
           (.delete (first lfs))
           (if (< (count (rest lfs)) lfc)
             (get-new-log-file (rest lfs))
             (recur (rest lfs)))))))
  ([_]
     (let [pfx (.format (SimpleDateFormat. "yyMMdd-HHmmss") (Date.))
           lfn (File. (get-dop-log-dir) (str (get-default :dop :log :file-body-name) \- pfx ".log"))]
       lfn)))

(defn- -create-logging-properties
  []
  (let [lf (get-new-log-file)]
    (reset-openar-log-file lf)
    (str (println-str "java.util.logging.FileHandler.pattern =" (.getCanonicalPath lf)))))

(defn get-logging-properties-inputstream
  [cfgs]
  (let [rld (get-resources-dir (get-default :src :resources :logger :dir-name))
        lcf (File. rld (str (get-default :src :resources :logger :configuration-file)))]
    (ByteArrayInputStream. (.getBytes (str (slurp lcf :encoding "UTF-8") \newline cfgs)))))

(defn- -get-logger?
  []
  (if *openar-logger*
    true
    (let [cfgs (-create-logging-properties)
          logger-name (str (get-default :dop :log :logger-name))
          logger-header (get-default :dop :log :logger-header)]
      (with-open [is (get-logging-properties-inputstream cfgs)]
        (.readConfiguration (LogManager/getLogManager) is)
        (reset-openar-logger logger-name logger-header (Logger/getLogger logger-name))
        (when-not *openar-logger*
          (throw (RuntimeException. "get a logger failed")))
        (reset-openar-logger-popup Level/WARNING)
        true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -save-std-inouterr?
  []
  (reset-standard-in-out-err *in* *out* *err*)
  true)

(defn- -redirect-system-out-and-err?
  []
  (when *openar-logger*
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

(defn install-thread-default-uncaught-exception-handler?
  []
  (when-not *thread-default-uncaught-exception-handler*
    (reset-thread-default-uncaught-exception-handler (proxy [Thread$UncaughtExceptionHandler] []
                                                       (uncaughtException [t e]
                                                                          (log-uncaught-exception t e)))))
  (Thread/setDefaultUncaughtExceptionHandler *thread-default-uncaught-exception-handler*)
  true)

(defn get-dop-openar-exception-dir?
  []
  (if (.isDirectory (get-dop-openar-exception-dir)) true false))

(defn load-exception-listeners?
  []
  (load-exception-listeners)
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn shutdown-log?
  []
  true)

(defn startup-log?
  []
  (with-create-on-get-dir
    (and true
         (-save-std-inouterr?)
         (get-dop-log-dir?)
         (cleanup-dop-log-dir?)
         (-get-logger?)
         (-redirect-system-out-and-err?)
         (install-thread-default-uncaught-exception-handler?)
         (get-dop-openar-exception-dir?)
         (load-exception-listeners?))))
