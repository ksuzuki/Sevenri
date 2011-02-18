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

(ns oplix.incantea.ui
  (:use [openar log oplix ui]
        [oplix.incantea core defs listeners])
  (:import (java.awt.event WindowAdapter)
           (java.util Vector)
           (javax.swing JDesktopPane JFrame JInternalFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-incanter)
(def *view-comparator* (fn [v1 v2] (.compareTo (.getName v1) (.getName v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn put-desktop
  ([desktop]
     (when *oplix*
       (put-desktop desktop *oplix*)))
  ([desktop oplix]
     (when (and (get-oplix oplix)
                (instance? JDesktopPane desktop))
       (put-oplix-prop oplix :desktop desktop))))

(defn get-desktop
  ([]
     (when *oplix*
       (get-desktop *oplix*)))
  ([oplix]
     (when (get-oplix oplix)
       (get-oplix-prop oplix :desktop))))

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
  (when *oplix*
    (let [ovs (filter (fn [[o v]] (identical? o *oplix*)) (xref-with :incantea-file))]
      (when (seq ovs)
        (when (< 1 (count ovs))
          (log-warning "incantea: inconsistent xref: an incantea is working on multiple spells"))
        (get-spell-name-from-spell-file (second (first ovs)))))))

(defn get-incantea-repl-name
  [oplix-name]
  (format "%s %s" oplix-name "Repl"))

(defn close-incantea-repl
  [oplix-name]
  (when-let [rn (get-incantea-repl-name oplix-name)]
    (when-let [ro (get-oplix rn)]
      (close-oplix ro))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "view")
