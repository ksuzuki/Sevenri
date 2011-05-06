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

(ns ^{:doc "Sevenri system-wide defs. There are also 'reset' fns defined for defs
which can be redefined."}
  sevenri.defs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; config

(def *user-dir* (System/getProperty "user.dir"))

(def *user-home* (System/getProperty "user.home"))

(def *dsr-path* nil)

(defn reset-dsr-path
  [path]
  (def *dsr-path* path))

(def *sid-name* nil)

(defn reset-sid-name
  [name]
  (def *sid-name* name))

(def *sid-path* nil)

(defn reset-sid-path
  [path]
  (def *sid-path* path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; core

(def *project-manager* nil)

(defn reset-project-manager
  [manager]
  (def *project-manager* manager))

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
;;;; props

(def *properties* nil)

(defn reset-properties
  [properties]
  (def *properties* properties))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix

(def *base-class-loader* nil)

(defn reset-base-class-loader
  [loader]
  (def *base-class-loader* loader))

(def *system-event-queue* nil)

(defn reset-system-event-queue
  [event-queue]
  (def *system-event-queue* event-queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ui

(def *event-delegator-class* nil)

(defn reset-event-delegator-class
  [evtdelegator-name]
  (def *event-delegator-class*
    (Class/forName (str evtdelegator-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

(def *saved-dynaclass-listeners* '_*edl*_)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

;; The order is important. Don't change unless it's necessary.
(def *startup-order* '[log props os jvm core slix ui debug])
