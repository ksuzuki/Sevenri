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

(ns sevenri.main
  (:use [sevenri core debug defs event log slix]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -open-sesami
  []
  (startup)
  (open-slix-sevenri-and-wait)
  (send-sevenri-starting-event)
  ;;
  (send-slixes-opening-event)
  (open-all-slixes-and-wait true)
  (post-slixes-opened-event))

(defn ok-to-quit?
  []
  (try
    (when (all-slixes-ok-to-quit?)
      (unlock-and-resume quit-lock))
    false
    (catch Exception e
      (log-exception e)
      (unlock-and-resume quit-lock)
      false)))

(defn- -close-sesami
  []
  (send-slixes-closing-event)
  (close-all-slixes-and-wait true)
  (send-slixes-closed-event)
  ;;
  (send-sevenri-quitting-event)
  (close-slix-sevenri-and-wait)
  (shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run
  [& args]
  (intern 'clojure.core '*command-line-args* (first args))
  (reset-ok-to-quit-fn ok-to-quit?)
  (run-swank-repl)
  (try
    (when-not (.exists (get-sevenri-lock-file))
      (-open-sesami)
      (lock-and-wait quit-lock)
      (-close-sesami))
    (System/exit 0)
    (catch Exception e
      (log-exception e))))
