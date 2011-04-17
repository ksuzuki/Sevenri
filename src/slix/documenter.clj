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
  slix.documenter
  (:use [sevenri config core log slix ui]
        [slix.documenter init keymap ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frame-created
  [event]
  (add-ui))

(defn frame-loaded
  [event]
  (restore-ui))

(defn opened
  [event]
  (add-panel-ui)
  (initialize)
  (set-slix-visible))

(defn saving
  [event]
  (save-current-work)
  (reset-editor-keymap (slix-frame))
  (save-dyna-listeners
   [[(slix-frame) [(listener-triplet Window)]]])
  (remove-toolboxes))

(defn saved
  [event]
  (add-toolboxes)
  (add-keymap-to-editor (slix-frame)))

(defn error-save
  [event]
  (add-toolboxes)
  (add-keymap-to-editor (slix-frame)))
