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

(ns ^{:doc "Sevenri main library - the run fn starts everything."}
  sevenri.main
  (:use [sevenri config debug event log startup]
        [sevenri.defs :only (reset-ok-to-quit-fn)]
        [sevenri.core :only (create-sid-sevenri-lock-file?
                             lock-and-wait
                             unlock-and-resume)]
        [sevenri.slix :only (open-slix-sevenri-and-wait
                             open-all-slixes-and-wait
                             close-all-slixes-and-wait
                             close-slix-sevenri-and-wait)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *quit-lock* (proxy [Object][]))

;;;;

(defn- -open-sesami
  []
  (startup-or-shutdown :startup)
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
      (unlock-and-resume *quit-lock*))
    false
    (catch Exception e
      (log-exception e)
      (unlock-and-resume *quit-lock*)
      false)))

(defn- -close-sesami
  []
  (send-slixes-closing-event)
  (close-all-slixes-and-wait true)
  (send-slixes-closed-event)
  ;;
  (send-sevenri-quitting-event)
  (close-slix-sevenri-and-wait)
  (startup-or-shutdown :shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run
  [& args]
  (intern 'clojure.core '*command-line-args* (first args))
  (reset-ok-to-quit-fn ok-to-quit?)
  (try
    (create-sid*)
    (when (create-sid-sevenri-lock-file?)
      (-open-sesami)
      (lock-and-wait *quit-lock*)
      (-close-sesami))
    (catch Exception e
      (log-exception e))
    (finally
      (System/exit 0))))
