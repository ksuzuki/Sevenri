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

(ns sevenri.defs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; config

(def *sid-dir* nil)

(defn reset-sid-dir
  [dir]
  (def *sid-dir* dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; debug

(def *swank-repl-is-running* false)

(defn reset-swank-repl-state
  [running?]
  (def *swank-repl-is-running* running?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jvm

(def *Sun-awt-is-available* (try
                              (if (and (Class/forName "sun.awt.AppContext")
                                       (Class/forName "sun.awt.SunToolkit"))
                                true
                                false)
                              (catch Exception e
                                false)))

(def *awt-utilities-available* (try
                                 (if (Class/forName "com.sun.awt.AWTUtilities")
                                   true
                                   false)
                                 (catch Exception e
                                   false)))

;;;;

(def *system-app-context* nil)

(defn reset-system-app-context
  [app-context]
  (def *system-app-context* app-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; log

(def *sevenri-logger-name* "Sevenri-logger")
(def *sevenri-logger-header* "sevenri:")
(def *sevenri-logger* nil)

(defn reset-sevenri-logger
  [name header logger]
  (def *sevenri-logger-name* name)
  (def *sevenri-logger-header* header)
  (def *sevenri-logger* logger))

(def *sevenri-logger-popup-level* java.util.logging.Level/WARNING)
(def *sevenri-logger-popup-sec* 3)

(defn reset-sevenri-logger-popup
  ([level]
     (def *sevenri-logger-popup-level* level))
  ([level sec]
     (def *sevenri-logger-popup-level* level)
     (def *sevenri-logger-popup-sec* sec)))

(defn get-sevenri-logger-popup-level
  []
  (cond
   (instance? java.util.logging.Level *sevenri-logger-popup-level*)
     (.intValue *sevenri-logger-popup-level*)
   (instance? Integer *sevenri-logger-popup-level*)
     *sevenri-logger-popup-level*
   :else
     (.intValue java.util.logging.Level/OFF)))

;;;;

(def *sevenri-log-file* nil)

(defn reset-sevenri-log-file
  [log-file]
  (def *sevenri-log-file* log-file))

;;;;

(def *standard-in*  nil)
(def *standard-out* nil)
(def *standard-err* nil)

(defn reset-standard-in-out-err
  [in out err]
  (def *standard-in* in)
  (def *standard-out* out)
  (def *standard-err* err))

;;;;

(def *thread-default-uncaught-exception-handler* nil)

(defn reset-thread-default-uncaught-exception-handler
  [handler]
  (def *thread-default-uncaught-exception-handler* handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main

(def *ok-to-quit-fn* nil)

(defn reset-ok-to-quit-fn
  [ok-to-quit-fn]
  (def *ok-to-quit-fn* ok-to-quit-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix

(def *frame-location-by-platform* true)

(defn reset-frame-location-by-platform
  [by-platform?]
  (def *frame-location-by-platform* by-platform?))

(def *base-class-loader* nil)

(defn reset-base-class-loader
  [loader]
  (def *base-class-loader* loader))

(def *system-event-queue* nil)

(defn reset-system-event-queue
  [event-queue]
  (def *system-event-queue* event-queue))

(def *slix-sevenri-can-close* false)

(defn reset-slix-sevenri-can-close
  [can?]
  (def *slix-sevenri-can-close* can?))

(def *log-xml-encoder-errors* false)

(defn reset-log-xml-encoder-errors
  [log?]
  (def *log-xml-encoder-errors* log?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ui

(def *event-delegator-class* nil)

(defn reset-event-delegator-class
  [evtdelegator-name]
  (def *event-delegator-class*
    (Class/forName (str evtdelegator-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup

;; The order is important. Don't change unless it's necessary.
(def *startup-order* '[log jvm os core slix ui])

(defn using-fn-prefix
  [fquns]
  (let [uns (if-let [m (re-matches #"^sevenri\.(.*)$" (str fquns))]
              (second m)
              fquns)]
    (str uns "-using-")))

(defmacro using-fns
  "uns is using rfns of rns. Expand to defns of uns-using-rfn-uns"
  [uns rns rfns]
  (let [ufp (using-fn-prefix uns)
        dfs (map #(list 'defn
                        (symbol (str ufp % \- rns))
                        '[& args]
                        (vector (list 'quote %) (list 'quote rns)))
                 rfns)]
    `(do ~@dfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ui

(def *saved-dynaclass-listeners* '_*edl*_)
