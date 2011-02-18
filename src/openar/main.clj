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

(ns openar.main
  (:use [openar core debug defs event log oplix]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -open-sesami
  []
  (startup)
  (open-oplix-openar-and-wait)
  (send-openar-starting-event)
  ;;
  (send-oplixes-opening-event)
  (open-all-oplixes-and-wait true)
  (post-oplixes-opened-event))

(defn ok-to-quit?
  []
  (try
    (when (all-oplixes-ok-to-quit?)
      (unlock-and-resume quit-lock))
    false
    (catch Exception e
      (log-exception e)
      (unlock-and-resume quit-lock)
      false)))

(defn- -close-sesami
  []
  (send-oplixes-closing-event)
  (close-all-oplixes-and-wait true)
  (send-oplixes-closed-event)
  ;;
  (send-openar-quitting-event)
  (close-oplix-openar-and-wait)
  (shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run
  [& args]
  (intern 'clojure.core '*command-line-args* (first args))
  (reset-ok-to-quit-fn ok-to-quit?)
  (run-swank-repl)
  (try
    (when-not (.exists (get-openar-lock-file))
      (-open-sesami)
      (lock-and-wait quit-lock)
      (-close-sesami))
    (System/exit 0)
    (catch Exception e
      (log-exception e))))
