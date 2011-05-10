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

(ns ^{:doc "Sevenri defs"}
  sevenri.defs)

(defmacro redef!
  "This macro should be called only at Sevenri startup time to alter the
   system defs."
  [var val]
  `(alter-var-root #'~var (fn [~'_] ~val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; config

(def *user-dir* (System/getProperty "user.dir"))
(def *user-home* (System/getProperty "user.home"))

(def *config* nil)

(def *dsr-path* nil)
(def *sid-name* nil)
(def *sid-path* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; core

(def *project-manager* nil)

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

(def *system-app-context* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; log

(def *sevenri-logger* nil)
(def *sevenri-logger-name* "Sevenri-logger")
(def *sevenri-logger-popup-level* java.util.logging.Level/WARNING)
(def *sevenri-logger-popup-sec* 3)

(def *sevenri-log-file* nil)

;;;;

(def *standard-in*  nil)
(def *standard-out* nil)
(def *standard-err* nil)

(def *thread-default-uncaught-exception-handler* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main

(def *ok-to-quit-fn* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; props

(def *properties* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix

(def *base-class-loader* nil)

(def *system-event-queue* nil)

(def *path-watcher* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ui

(def *event-delegator-class* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

(def *saved-dynaclass-listeners* '_*edl*_)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

;; The order is important. Don't change unless it's necessary.
(def *startup-order* '[log props os jvm core slix ui debug])
