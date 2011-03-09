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

(ns slix.planter.defs
  (:use [sevenri log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *button-size* [96 29])
(def *name-config-map* "name-config-map")
(def *preferred-fonts* [["Inconsolata" 0 14] ["Courier" 0 14]])
(def *xref-planter-project* :planter-project)

(def *more-actions* ["More Actions..."
                     "Clean"
                     "Delete..."
                     "Deps"
                     "Install"
                     "New..."
                     "Pom"
                     "Test"
                     "Test!"
                     "UberJar"])
