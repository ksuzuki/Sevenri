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

(ns oplix.ced.caret
  (:gen-class
   :extends javax.swing.text.DefaultCaret
   :exposes-methods {adjustVisibility superAdjustVisibility}
   :methods [[setAdjustVisibility [java.lang.Boolean] java.lang.Boolean]
             [isAdjustingVisibility [] java.lang.Boolean]]
   :state caretState
   :init initCaretState
   :main false))

(defn -initCaretState
  []
  [[] (atom true)])

(defn -setAdjustVisibility
  [this set?]
  (reset! (.caretState this) set?))

(defn -isAdjustingVisibility
  [this]
  (deref (.caretState this)))

(defn -adjustVisibility
  [this nloc]
  (when (deref (.caretState this))
    (.superAdjustVisibility this nloc)))
