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

(ns ^{:oplix true}
  oplix.repl
  (:require clojure.main)
  (:use [openar config core debug event jvm log oplix os ui utils]
        [oplix.repl core ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; oplix event handlers

(defn frame-created
  [event]
  (create-repl-frame))

(defn opened
  [event]
  (start-repl)
  (set-oplix-visible))

(defn saving
  [event]
  (saving-repl event))

(defn saved
  [event]
  (saved-repl event))

(defn closing
  [event]
  (end-repl event))
