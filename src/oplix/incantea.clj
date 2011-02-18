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
  oplix.incantea
  (:use [openar config core event log oplix ui utils]
        [oplix.incantea core init ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-incanter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frame-created
  [event]
  (add-ui))

(defn opened
  [event]
  (download-datasets)
  (init-ui)
  (set-oplix-visible))

(defn saving
  [event]
  (save-incantea event))

(defn closing
  [event]
  (remove-all-sketches)
  (close-incantea-repl (oplix-name)))
