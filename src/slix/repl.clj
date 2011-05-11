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

(ns ^{:slix true}
  slix.repl
  (:require clojure.main)
  (:use [sevenri config core debug event jvm log os slix ui utils]
        [slix.repl core ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn frame-created
  [event]
  (create-repl-frame))

(defn opened
  [event]
  (ui-initialize)
  (start-repl)
  (set-slix-visible))

(defn saving
  [event]
  (saving-repl event))

(defn saved
  [event]
  (saved-repl event))

(defn closing
  [event]
  (end-repl event))
