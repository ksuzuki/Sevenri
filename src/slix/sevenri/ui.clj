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
  (:use [sevenri props slix])
  (:import (java.awt Cursor Dimension Toolkit)
           (slix.sevenri.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-main-panel-components
  [frame]
  (let [fprops (frame-props frame)]
    (or (get-prop fprops 'main.panel.components)
        (let [mp (.getComponent (.getContentPane frame) 0)
              mpcs {:main-panel mp
                    :label-Sevenri (.getLblSevenri mp)
                    :list-sn (.getLstSn mp) :divider (.getSpDivider mp) :list-name (.getLstName mp)}]
          (put-prop fprops 'mail.panel.components mpcs)
          mpcs))))

(defn create-sevenri-frame
  []
  (let [frame (slix-frame)
        [minw minh] (read-prop (get-props) 'slix.frame.size)
        [stdw stdh] (read-prop (slix-props) 'frame.standard.size)
        scrnsize (.getScreenSize (Toolkit/getDefaultToolkit))
        posx (max 0 (- (.width scrnsize) stdw))]
    (.add (.getContentPane frame) (MainPanel.))
    (get-main-panel-components frame)
    (doto frame
      (.setMinimumSize (Dimension. minw minh))
      (.setSize stdw stdh)
      (.setLocation posx 0))))

(defn enable-main-panel
  [frame enable?]
  (.setEnabled (:main-panel (get-main-panel-components frame)) (if enable? true false)))

(defn set-divider
  ([]
     (set-divider (read-prop (slix-props) 'divider.initial.location)))
  ([loc]
     (let [mpcs (get-main-panel-components (slix-frame))]
       (.setDividerLocation (:divider mpcs) (double loc)))))

(defn show-slixes-event-cursor
  [show?]
  (let [frame (slix-frame)
        fprps (frame-props frame)]
    (dosync
     (if show?
       (if-let [cc (get-prop fprps 'cursor)]
         (ref-set cc [(first @cc) (inc (second @cc))])
         (let [cc (ref [(.getCursor frame) 1])]
           (put-prop fprps 'cursor cc)
           (.setCursor frame Cursor/WAIT_CURSOR)))
      ;;
      (when-let [cc (get-prop fprps 'cursor)]
        (if (zero? (dec (second @cc)))
          (do
            (remove-prop fprps 'cursor)
            (.setCursor frame (first @cc)))
          (ref-set cc [(first @cc) (dec (second @cc))])))))))
