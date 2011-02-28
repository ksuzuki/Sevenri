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

(ns slix.incantea.defs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *incanter-home* "incanter.home")
(def *sample-datasets* 'sample-datasets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *new-spell-name* "new-spell")
(def *spell-ns* 'spell)

(def *initial-frame-width* 500)
(def *initial-frame-height* 300)

(def *actions-width* 200)
(def *action-items* {:actions "Actions..."
                     :new-spell "New Spell"
                     :delete "Delete..."})

(def *views-width* 240)
(def *views-title* "Views...")
(def *views-items* {:close-all "Close All..."
                    :fit-in "Fit In"
                    :fit-in-all "Fit In All"
                    :iconify-all "Iconfiy All"
                    :deiconify-all "Deiconfiy All"
                    :tile-horizontally "Tile Horizontally"
                    :tile-vertically "Tile Vertically"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *prop-main-controls* "main-controls")
(def *prop-ref-views* "ref-views")
(def *prop-ref-sketches* "ref-sketches")
