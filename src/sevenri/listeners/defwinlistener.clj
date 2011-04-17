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

(ns ^{:doc "Sevenri default window listener class"}
  sevenri.listeners.defwinlistener
  (:gen-class
   :extends java.awt.event.WindowAdapter
   :state defWinListeners
   :init initDefWinListeners
   :methods [[setDefWinListeners [clojure.lang.PersistentArrayMap] void]
             [getDefWinListeners [] clojure.lang.PersistentArrayMap]]
   :main false)
  (:use [sevenri.ui :only (def-listener-method get-default-window-listeners)]))

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

(def-listener-method defWinListeners windowActivated)
(def-listener-method defWinListeners windowClosed)
(def-listener-method defWinListeners windowClosing)
(def-listener-method defWinListeners windowDeactivated)
(def-listener-method defWinListeners windowDeiconified)
(def-listener-method defWinListeners windowGainedFocus)
(def-listener-method defWinListeners windowIconified)
(def-listener-method defWinListeners windowLostFocus)
(def-listener-method defWinListeners windowOpened)
(def-listener-method defWinListeners windowStateChanged)
