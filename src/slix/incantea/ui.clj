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

(ns slix.incantea.ui
  (:use [sevenri log slix ui]
        [slix.incantea core defs listeners])
  (:import (java.awt.event WindowAdapter)
           (java.util Vector)
           (javax.swing JDesktopPane JFrame JInternalFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-incanter)
(def *view-comparator* (fn [v1 v2] (.compareTo (.getName v1) (.getName v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn put-desktop
  ([desktop]
     (when *slix*
       (put-desktop desktop *slix*)))
  ([desktop slix]
     (when (and (get-slix slix)
                (instance? JDesktopPane desktop))
       (put-slix-prop slix :desktop desktop))))

(defn get-desktop
  ([]
     (when *slix*
       (get-desktop *slix*)))
  ([slix]
     (when (get-slix slix)
       (get-slix-prop slix :desktop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -save-main-controls
  [control-map]
  (when-let [desktop (get-desktop)]
    (.putClientProperty desktop *prop-main-controls* control-map)))

(defn -get-main-controls
  []
  (when-let [desktop (get-desktop)]
    (.getClientProperty desktop *prop-main-controls*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-spell-name
  []
  (when *slix*
    (let [ovs (filter (fn [[o v]] (identical? o *slix*)) (xref-with :incantea-file))]
      (when (seq ovs)
        (when (< 1 (count ovs))
          (log-warning "incantea: inconsistent xref: an incantea is working on multiple spells"))
        (get-spell-name-from-spell-file (second (first ovs)))))))

(defn get-incantea-repl-name
  [slix-name]
  (format "%s %s" slix-name "Repl"))

(defn close-incantea-repl
  [slix-name]
  (when-let [rn (get-incantea-repl-name slix-name)]
    (when-let [ro (get-slix rn)]
      (close-slix ro))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "view")
