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

(ns ^{:slix true :singleton true}
  slix.sevenri
  (:use [sevenri config core event log os slix ui utils])
  (:use [slix.sevenri aotlist lists ui])
  (:import (java.awt Dimension Toolkit)
           (java.io File)
           (slix.sevenri.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-lists
  []
  (let [mpcs (get-main-panel-components (slix-frame))]
    (future
      (let [lstSn (:lstSn mpcs)
            sns (sort (map str (get-all-slix-sn)))
            sis (get-selected-indices (seq (.getSelectedValues lstSn)) sns)]
        (invoke-later #(doto lstSn
                         (.setListData (into-array String sns))
                         (.setSelectedIndices (int-array sis))))))))

(defn aot-compile-slixes
  []
  (let [wmsg "sevenri: aot-compile-slixes: aot-compile failed for:"]
    (doseq [sn *aot-compile-list*]
      (if (aot-compile? sn 'aot)
        (when-let [aot-os (cond
                           (is-mac?) 'aot-mac
                           :else nil)]
          (when (.exists (File. (get-slix-dir sn) (str (nssym2path aot-os) ".clj")))
            (when-not (aot-compile? sn aot-os)
              (log-warning wmsg aot-os))))
        (log-warning wmsg sn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn frame-created
  [event]
  (let [fr (slix-frame)
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
  (set-slix-visible)
  (setup-main-panel true)
  (aot-compile-slixes)
  (add-to-xref *slix* :update-lists-fn #'update-lists))

(defn saving
  [event]
  (let [mpcs (get-main-panel-components (slix-frame))]
    (save-dyna-listeners
     [[(:lstSn mpcs) [(listener-triplet ListSelection)
                      (listener-triplet Mouse)]]
      [(:lstName mpcs) [(listener-triplet ListSelection)
                        (listener-triplet Mouse)]]])))

(defn closing
  [event]
  (when-not (can-slix-sevenri-close?)
    (create-event-response
     :sevenri.event/response-donot-close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn slix-created
  [event]
  (update-lists))

(defn slix-opened
  [event]
  (update-lists))

(defn slix-closed
  [event]
  (update-lists))

(defn slix-deleted
  [event]
  (update-lists))

(defn slix-purged
  [event]
  (update-lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn slixes-opening
  [event]
  (show-slixes-event-cursor true))

(defn slixes-opened
  [event]
  (show-slixes-event-cursor false))

(defn slixes-closing
  [event]
  (show-slixes-event-cursor true))

(defn slixes-closed
  [event]
  (show-slixes-event-cursor false))
