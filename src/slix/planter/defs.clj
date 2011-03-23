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

(def *min-frame-size* [550 340])
(def *button-size* [96 29])
(def *cursor* "cursor")
(def *name-config-map* "name-config-map")
(def *preferred-fonts* [["Inconsolata" 0 14] ["Courier" 0 14]])
(def *slix-planter-project* 'slix.planter)
(def *xref-planter-project* :planter-project)

(def *lein-commands* #{"clean" "compile" "deps" "install" "jar"
                       "jutest" "jutest!"
                       "new" "pom" "test!" "test" "uberjar"})

(def *standard-actions* ["Edit"])

(def *more-actions* ["More Actions..."
                     "Clean"
                     "Clone..."
                     "Delete..."
                     "Deps"
                     "Install"
                     "New..."
                     "Pom"
                     "Rename..."
                     "Test!"
                     "UberJar"])

(def *command-aliases* {"test" "jutest"
                        "test!" "jutest!"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *lein-loaded* (atom false))

(defn lein-loaded
  ([]
     @*lein-loaded*)
  ([b]
     (reset! *lein-loaded* true)
     b))

(def *shutdown-lein* nil)

(defn shutdown-lein
  ([]
     *shutdown-lein*)
  ([fnc]
     (when (fn? fnc)
       (def *shutdown-lein* fnc))))
