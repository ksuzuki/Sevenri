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
  (:use [sevenri config core event log os slix ui])
  (:use [slix.sevenri aotlist lists ui]))

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
                         (.setSelectedIndices (into-array Integer/TYPE sis))))))))

(defn aot-compile-slixes
  []
  (let [frame (slix-frame)]
    (future
      (let [msg "sevenri: aot-compile-slixes: aot-compile failed for:"]
        (enable-main-panel frame false)
        ;;
        (doseq [sn *aot-compile-list*]
          (if (aot-compile? sn 'aot)
            (when-let [aot-os (cond
                               (is-mac?) 'aot-mac
                               :else nil)]
              (when (.exists (get-slix-path sn (str aot-os '!clj)))
                (when-not (aot-compile? sn aot-os)
                  (log-warning msg aot-os))))
            (log-warning msg sn)))
        ;; 
        (enable-main-panel frame true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn frame-created
  [event]
  (post-frame-created))

(defn opened
  [event]
  (setup-main-panel [(get-sn-list-listener) (get-sn-list-mouse-listener)
                     (get-name-list-listener) (get-name-list-mouse-listener)])
  (set-slix-visible)
  (update-divider)
  (aot-compile-slixes)
  (update-lists)
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
    (event-response-donot-close)))

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
