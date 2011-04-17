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

(ns slix.documenter.mddb
  (:use [clojure set]
        [sevenri config core log utils]
        [slix.documenter io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *mddb*
;; {:categories #{categoryA categoryB ...}
;;
;;  :categoryA #{titleX titleY ...}
;;  :categoryB #{titleX titleY ...}}
;;
;;  :titleX-categoryA #{section1 section2 ...}
;;  :titleY-categoryB #{section1 section2 ...}

(def *mddb* (atom {})) ;; metadoc database

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-mddb
  []
  @*mddb*)

(defn set-mddb
  [mddb]
  (reset! *mddb* mddb))

(defn clear-mddb
  []
  (reset! *mddb* {}))

(defn update-mddb
  []
  (let [cds (map #(get-doc-path (symbol %)) (:categories (read-root-metadoc)))
        rep (re-pattern (str ".*/" *metadoc-file-name* "$"))]
    (loop [mddb {}
           mdfs (flatten (map (fn [cd] (find-files #(re-matches rep (str %)) cd)) cds))]
      (if (seq mdfs)
        (let [mtdc (read-metadoc (first mdfs))
              category (:category mtdc)
              title (:title mtdc)
              sections (:sections mtdc)
              ;;
              categories-val (let [cv (set (:categories mddb))]
                               (if (and category (get-category category))
                                 (conj cv category)
                                 cv))
              ;;
              categoryA-key (when category
                              (keyword category))
              categoryA-val (when category
                              (let [cav (set (categoryA-key mddb))]
                                (if (and title (get-title title category))
                                  (conj cav title)
                                  cav)))
              ;;
              titleX-categoryA-key (when (and category title)
                                     (keyword (str title \- category)))
              titleX-categoryA-val (when (and category title)
                                     (let [catxv (set (titleX-categoryA-key mddb))]
                                       (if sections
                                         (union catxv (set (filter #(get-src-section % title category)
                                                              sections)))
                                         catxv)))]
          ;;
          (recur (-> mddb
                     (merge {:categories categories-val})
                     (merge (if categoryA-val
                              {categoryA-key categoryA-val}
                              {}))
                     (merge (if titleX-categoryA-val
                              {titleX-categoryA-key titleX-categoryA-val}
                              {})))
                 (rest mdfs)))
        ;;
        (set-mddb mddb)))))

;;;;

(defn get-categories
  []
  (:categories (get-mddb)))

(defn get-titles
  [category]
  ((keyword (str category)) (get-mddb)))

(defn get-sections
  [title category]
  ((keyword (str title \- category)) (get-mddb)))
