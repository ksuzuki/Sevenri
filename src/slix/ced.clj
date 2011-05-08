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
  slix.ced
  (:use [sevenri event slix]
        [slix.ced init]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn opening
  [event]
  (ced-opening event))

(defn frame-created
  [event]
  (create-ced-frame))

(defn opened
  [event]
  (ced-opened event)
  (set-slix-visible))

(defn saving
  [event]
  (event-response-donot-save))

(defn closing
  [event]
  (ced-closing event))
