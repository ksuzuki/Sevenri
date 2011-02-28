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

(ns sevenri.listeners.defkeylistener
  (:gen-class
   :extends java.awt.event.KeyAdapter
   :state defKeyListeners
   :init initDefKeyListeners
   :methods [[setDefKeyListeners [clojure.lang.PersistentArrayMap] void]
             [getDefKeyListeners [] clojure.lang.PersistentArrayMap]]
   :main false)
  (:use [sevenri ui]
        [sevenri.listeners deflistener]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -initDefKeyListeners
  []
  [[] (atom (get-default-key-listeners))])

(defn -setDefKeyListeners
  [this listeners]
  (reset! (.defKeyListeners this) listeners))

(defn -getDefKeyListeners
  [this]
  @(.defKeyListeners this))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-method defKeyListeners keyPressed)
(def-method defKeyListeners keyReleased)
(def-method defKeyListeners keyTyped)
