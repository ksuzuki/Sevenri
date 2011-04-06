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
  (:use [sevenri config core slix ui]
        [slix.sevenri defs])
  (:import (java.awt Cursor Dimension Toolkit)
           (slix.sevenri.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn post-frame-created
  []
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

(defn get-main-panel-components
  [frame]
  (let [mp (.getComponent (.getContentPane frame) 0)]
    {:mainPanel mp
     :lblSevenri (.getLblSevenri mp)
     :spDivider (.getSpDivider mp) :lstSn (.getLstSn mp) :lstName (.getLstName mp)}))

(defn enable-main-panel
  [frame enable?]
  (.setEnabled (:mainPanel (get-main-panel-components frame)) enable?))

(defn setup-main-panel
  [list-listeners]
  (let [mpcs (get-main-panel-components (slix-frame))
        [sn-list-listener sn-list-mouse-listener
         nm-list-listener nm-list-mouse-listener] list-listeners]
    (.setText (:lblSevenri mpcs) (get-sevenri-name-and-version))
    (.addListSelectionListener (:lstSn mpcs) sn-list-listener)
    (.addMouseListener (:lstSn mpcs) sn-list-mouse-listener)
    (.addListSelectionListener (:lstName mpcs) nm-list-listener)
    (.addMouseListener (:lstName mpcs) nm-list-mouse-listener)
    (doseq [c [(:lstSn mpcs) (:lstName mpcs)]]
      (add-default-key-listener c))))

(defn update-divider
  ([]
     (update-divider 0.35))
  ([pos]
     (let [mpcs (get-main-panel-components (slix-frame))]
       (.setDividerLocation (:spDivider mpcs) (double pos)))))

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
