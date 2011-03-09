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
  slix.planter
  (:use [sevenri config core event log slix ui utils]
        [slix.planter controller core init io ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn opened
  [event]
  (add-ui)
  (set-slix-visible)
  (verify)
  (when (is-planter-project-ready?)
    (when-let [pn (get-project *slix*)]
      (init-ui pn))))

(defn saving
  [event]
  (remove-ui)
  (save-state *slix* (get-project-name *slix*))
  ;; Shut up xml encoder error msgs caused by PopupFactory that is
  ;; auto-installed by Swing when popuping ComboBox items.
  (save-dyna-listeners
   [[(slix-frame) [(listener-triplet Window)]]]))

(defn saved
  [event]
  (restore-ui))
