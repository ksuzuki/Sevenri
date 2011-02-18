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

(ns ^{:oplix true :singleton true}
  oplix.openar
  (:use [openar config core event log oplix os ui utils])
  (:use [oplix.openar aotlist lists ui])
  (:import (java.awt Dimension Toolkit)
           (java.io File)
           (oplix.openar.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-lists
  []
  (let [mpcs (get-main-panel-components (oplix-frame))]
    (future
      (let [lstOn (:lstOn mpcs)
            ons (sort (map str (get-all-oplix-on)))
            sis (get-selected-indices (seq (.getSelectedValues lstOn)) ons)]
        (invoke-later #(doto lstOn
                         (.setListData (into-array String ons))
                         (.setSelectedIndices (into-array Integer/TYPE sis))))))))

(defn aot-compile-oplixes
  []
  (let [wmsg "openar: aot-compile-oplixes: aot-compile failed for:"]
    (doseq [on *aot-compile-list*]
      (if (aot-compile? on 'aot)
        (when-let [aot-os (cond
                           (is-mac?) 'aot-mac
                           :else nil)]
          (when (.exists (File. (get-oplix-dir on) (str (nssym2path aot-os) ".clj")))
            (when-not (aot-compile? on aot-os)
              (log-warning wmsg aot-os))))
        (log-warning wmsg on)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; oplix event handlers

(defn frame-created
  [event]
  (let [fr (oplix-frame)
        cp (.getContentPane fr)
        mp (MainPanel.)]
    (.add cp mp)
    (let [minw (get-default :frame :width)
          minh (get-default :frame :height)
          stdw 400
          stdh 250
          ssiz (.getScreenSize (Toolkit/getDefaultToolkit))
          posx (max 0 (- (.width ssiz) stdw))]
    (doto fr
      (.setMinimumSize (Dimension. minw minh))
      (.setSize stdw stdh)
      (.setLocation posx 0)))))

(defn opened
  [event]
  (setup-main-panel)
  (update-lists)
  (set-oplix-visible)
  (setup-main-panel true)
  (aot-compile-oplixes)
  (add-to-xref *oplix* :update-lists-fn #'update-lists))

(defn saving
  [event]
  (let [mpcs (get-main-panel-components (oplix-frame))]
    (save-dyna-listeners
     [[(:lstOn mpcs) [(listener-triplet ListSelection)
                      (listener-triplet Mouse)]]
      [(:lstName mpcs) [(listener-triplet ListSelection)
                        (listener-triplet Mouse)]]])))

(defn closing
  [event]
  (when-not (can-oplix-openar-close?)
    (create-event-response
     :openar.event/response-donot-close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn oplix-created
  [event]
  (update-lists))

(defn oplix-opened
  [event]
  (update-lists))

(defn oplix-closed
  [event]
  (update-lists))

(defn oplix-deleted
  [event]
  (update-lists))

(defn oplix-purged
  [event]
  (update-lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn oplixes-opening
  [event]
  (show-oplixes-event-cursor true))

(defn oplixes-opened
  [event]
  (show-oplixes-event-cursor false))

(defn oplixes-closing
  [event]
  (show-oplixes-event-cursor true))

(defn oplixes-closed
  [event]
  (show-oplixes-event-cursor false))
