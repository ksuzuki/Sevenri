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

(ns library.slix.incantea.spells.sample1j
  (:use [slix.incantea core ui]))

;; Using core, charts, datasets, processing, and stats by default.
(use-incanter)

(def sales
  (dataset
   [:name :year :sold]
   [["ロブ" 1997 18]
    ["ロブ" 1998 15]
    ["ロブ" 1999 23]
    ["ロブ" 2000 15]
    ["シンディ" 1997 6]
    ["シンディ" 1998 12]
    ["シンディ" 1999 3]
    ["シンディ" 2000 6]
    ["マイク" 1997 13]
    ["マイク" 1998 11]
    ["マイク" 1999 11]
    ["マイク" 2000 11]
    ["アリス" 1997 3]
    ["アリス" 1998 5]
    ["アリス" 1999 9]
    ["アリス" 2000 17]
    ["ジョン" 1997 15]
    ["ジョン" 1998 17]
    ["ジョン" 1999 19]
    ["ジョン" 2000 21]]))

(def sales-2000 ($where {:year 2000} sales))
(def pc-2000 (with-data ($order :sold :desc sales-2000) (pie-chart ($ :name) ($ :sold)
                                                                   :legend true
                                                                   :title "2000 セールス2")))

(defn incant
  []
  (do-views :close-all-yes)
  (with-data ($order :sold :desc ($where {:year 2000} sales))
    (view (bar-chart ($ :name) ($ :sold)
                     :x-label "営業員"
                     :y-label "ロット"
                     :title "2000 セールス1")))
  ;;
  (with-data ($where ($fn [name] (or (= name "アリス") (= name "ジョン"))) sales)
    (view (bar-chart :year :sold :group-by :name :legend true
                     :x-label "年"
                     :y-label "ロット"
                     :title "比較1"))
    (view (line-chart :year :sold :group-by :name :legend true
                      :x-label "年"
                      :y-label "ロット"
                      :title "比較2")))
  ;;
  (do-views :tile-horizontally))

;; (Re)load the above in the spell.sample1j namespace. This should be the last line.
(create-spell-ns 'sample1j)
