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

(ns slix.sevenri.ui
  (:use [sevenri core slix ui]
        [slix.sevenri defs lists])
  (:import (java.awt Cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn setup-main-panel
  ([]
     (setup-main-panel false))
  ([update-divider-only?]
     (let [mpcs (get-main-panel-components (slix-frame))]
       (if update-divider-only?
         (.setDividerLocation (:spDivider mpcs) (double 0.35))
         (do
           (.setText (:lblSevenri mpcs) (get-sevenri-name-and-version))
           (.addListSelectionListener (:lstSn mpcs) (get-sn-list-listener))
           (.addMouseListener (:lstSn mpcs) (get-sn-list-mouse-listener))
           (.addListSelectionListener (:lstName mpcs) (get-name-list-listener))
           (.addMouseListener (:lstName mpcs) (get-name-list-mouse-listener))
           (doseq [c [(:lstSn mpcs) (:lstName mpcs)]]
             (add-default-key-listener c)))))))

(defn show-slixes-event-cursor
  [show?]
  (let [frame (slix-frame)]
    (dosync
     (if show?
       (if-let [cc (get-slix-prop *prop-cursor*)]
         (ref-set cc [(first @cc) (inc (second @cc))])
         (let [cc (ref [(.getCursor frame) 1])]
           (put-slix-prop *prop-cursor* cc)
           (.setCursor frame Cursor/WAIT_CURSOR)))
      ;;
      (when-let [cc (get-slix-prop *prop-cursor*)]
        (if (zero? (dec (second @cc)))
          (do
            (remove-slix-prop *prop-cursor*)
            (.setCursor frame (first @cc)))
          (ref-set cc [(first @cc) (dec (second @cc))])))))))
