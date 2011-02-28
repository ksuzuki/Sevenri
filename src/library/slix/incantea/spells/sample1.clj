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

(ns library.slix.incantea.spells.sample1
  (:use [slix.incantea core ui]))

;; Using core, charts, datasets, processing, and stats by default.
(use-incanter)

(def sales
  (dataset
   [:name :year :sold]
   [["rob" 1997 18]
    ["rob" 1998 15]
    ["rob" 1999 23]
    ["rob" 2000 15]
    ["cyndi" 1997 6]
    ["cyndi" 1998 12]
    ["cyndi" 1999 3]
    ["cyndi" 2000 6]
    ["mike" 1997 13]
    ["mike" 1998 11]
    ["mike" 1999 11]
    ["mike" 2000 11]
    ["alice" 1997 3]
    ["alice" 1998 5]
    ["alice" 1999 9]
    ["alice" 2000 17]
    ["john" 1997 15]
    ["john" 1998 17]
    ["john" 1999 19]
    ["john" 2000 21]]))

(def sales-2000 ($where {:year 2000} sales))
(def pc-2000 (with-data ($order :sold :desc sales-2000) (pie-chart ($ :name) ($ :sold)
                                                                   :legend true
                                                                   :title "2000 Sales2")))

(defn incant
  []
  (do-views :close-all-yes)
  (with-data ($order :sold :desc ($where {:year 2000} sales))
    (view (bar-chart ($ :name) ($ :sold)
                     :x-label "Sales Person"
                     :y-label "lot"
                     :title "2000 Sales1")))
  (with-data ($where ($fn [name] (or (= name "alice") (= name "john"))) sales)
    (view (bar-chart :year :sold :group-by :name :legend true
                     :x-label "year"
                     :y-label "lot"
                     :title "Comparison1"))
    (view (line-chart :year :sold :group-by :name :legend true
                      :x-label "year"
                      :y-label "lot"
                      :title "Comparison2")))
  (do-views :tile-horizontally))

;; (Re)load the above in the spell.sample1 namespace. This should be the last line.
(create-spell-ns 'sample1)
