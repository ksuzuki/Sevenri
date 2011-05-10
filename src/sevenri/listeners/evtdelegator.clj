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

(ns ^{:doc "Sevenri persistent event listener delegator class"}
  sevenri.listeners.evtdelegator
  (:gen-class
   :state state
   :init init
   :constructors {[java.lang.String][]}
   :methods [^{:static true} [getPersistenceDelegate [] java.lang.Object]
             [getId [] java.lang.String]
             [getHandler [] java.lang.Object]
             [setId [java.lang.String] void]
             [setHandler [java.lang.Object] void]
             [handleEvent [java.util.EventObject] void]]
   :main false))

(defn -getPersistenceDelegate
  []
  (proxy [java.beans.PersistenceDelegate] []
    (instantiate [old-instance out]
      (java.beans.Expression. old-instance
                              (.getClass old-instance)
                              "new"
                              (into-array [(.getId old-instance)])))))

(defn -init
  [id]
  [[] (atom [(str id) nil])])

(defn -getId
  [this]
  (first @(.state this)))

(defn -getHandler
  [this]
  (second @(.state this)))

(defn -setId
  [this id]
  (reset! (.state this) [(str id) (.getHandler this)]))

(defn -setHandler
  [this handler]
  (if handler
    (reset! (.state this) [(.getId this) handler])
    (throw (IllegalArgumentException. "evtdelegator: setHandler: null-handler"))))

(defn -handleEvent
  [this event]
  (when-let [handler (.getHandler this)]
    (handler event)))
