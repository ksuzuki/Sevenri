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

(ns ^{:slix true}
  slix.sevenri.about
  (:use [sevenri config core event log props slix ui]
        [slix.sevenri drawicon])
  (:import (java.awt BorderLayout Color Font Transparency)
           (java.awt.event MouseListener MouseEvent)
           (javax.imageio ImageIO)
           (javax.swing JLabel JPanel SwingConstants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *copyright-year-name* "2011 Kei Suzuki")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-sevenri-banner
  [canvas]
  (.setLayout canvas (BorderLayout.))
  (let [ver (JLabel. (str (get-sevenri-name-and-version) "  "))
        cpr (JLabel. (str "Copyright (C) "
                          *copyright-year-name*
                          "  All rights reserved."))
        clr (Color. 16rcc 16rcc 16r55)]
    (doto ver
      (.setFont (Font. "Times" Font/BOLD, 24))
      (.setHorizontalAlignment SwingConstants/RIGHT)
      (.setForeground clr))
    (doto cpr
      (.setFont (Font. "Times" Font/PLAIN 11))
      (.setHorizontalAlignment SwingConstants/LEFT)
      (.setForeground clr)) 
    (.add canvas ver BorderLayout/NORTH)
    (.add canvas cpr BorderLayout/SOUTH)))

(defn create-canvas
  []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (let [gt (.getTransform g)]
        (draw this g true)
        (.setTransform g gt)))))

(defn save-image
  [canvas]
  #_(lg "save-image canvas:" canvas)
  (let [cg (.getGraphics canvas)
        dc (.getDeviceConfiguration cg)
        bi (.createCompatibleImage dc 512 512 Transparency/TRANSLUCENT)
        gc (.createGraphics bi)
        fp (get-sid-temp-path 'sevenri-icon!png)]
    (draw bi gc)
    (.dispose gc)
    (ImageIO/write bi "png" fp)))

(defn add-mouse-listener
  [frame canvas]
  (set-listener-handlers frame MouseListener
                         {'mc ['mouseClicked
                               (fn [e]
                                 (if (pos? (bit-and MouseEvent/META_DOWN_MASK (.getModifiersEx e)))
                                   (save-image canvas)
                                   (.repaint canvas 0 0 0 (.getWidth canvas) (.getHeight canvas))))]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn frame-created
  [event]
  (.setSize (slix-frame) 300 320))

(defn opened
  [event]
  (let [fr (slix-frame)
        cp (.getContentPane fr)
        cv (create-canvas)]
    (add-mouse-listener fr cv)
    (add-sevenri-banner cv)
    (.add cp cv)
    (put-prop (frame-props fr) 'canvas cv))
  ;;
  (when (= (slix-name) "Sevenri.about")
    (set-slix-title "About Sevenri"))
  (set-slix-visible))

;;;;

(defmacro do-canvas-to-contentpane
  [op]
  `(. (.getContentPane (slix-frame)) ~op (get-prop (slix-frame-props) ~''canvas)))

(defn saving
  [event]
  ;; Suppress the 'proxy' canvas class not found err msg when the
  ;; xml-encoder err log is turned on.
  (do-canvas-to-contentpane remove))

(defn saved
  [event]
  ;; Add the canvas back on the content pane.
  (do-canvas-to-contentpane add))
