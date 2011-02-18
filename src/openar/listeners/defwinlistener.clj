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

(ns openar.listeners.defwinlistener
  (:gen-class
   :extends java.awt.event.WindowAdapter
   :state defWinListeners
   :init initDefWinListeners
   :methods [[setDefWinListeners [clojure.lang.PersistentArrayMap] void]
             [getDefWinListeners [] clojure.lang.PersistentArrayMap]]
   :main false)
  (:use [openar ui]
        [openar.listeners deflistener]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -initDefWinListeners
  []
  [[] (atom (get-default-window-listeners))])

(defn -setDefWinListeners
  [this handlers]
  (reset! (.defWinListeners this) handlers))

(defn -getDefWinListeners
  [this]
  @(.defWinListeners this))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-method defWinListeners windowActivated)
(def-method defWinListeners windowClosed)
(def-method defWinListeners windowClosing)
(def-method defWinListeners windowDeactivated)
(def-method defWinListeners windowDeiconified)
(def-method defWinListeners windowGainedFocus)
(def-method defWinListeners windowIconified)
(def-method defWinListeners windowLostFocus)
(def-method defWinListeners windowOpened)
(def-method defWinListeners windowStateChanged)
