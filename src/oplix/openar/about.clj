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

(ns ^{:oplix true}
  oplix.openar.about
  (:use [openar config core event log oplix ui utils]
        [oplix.openar drawicon])
  (:import (java.awt BorderLayout Color Font Transparency)
           (java.awt.event MouseAdapter MouseEvent)
           (java.io File)
           (javax.imageio ImageIO)
           (javax.swing JLabel JPanel SwingConstants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *copyright-year-name* "2011 Kei Suzuki")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-openar-banner
  [canvas]
  (.setLayout canvas (BorderLayout.))
  (let [ver (JLabel. (str (get-openar-name-and-version) "  "))
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
        g (.createGraphics bi)
        fp (File. (get-dop-temp-dir) "openar-icon.png")]
    (draw bi g)
    (.dispose g)
    (ImageIO/write bi "png" fp)))

(defn add-mouse-listener
  [frame canvas]
  (.addMouseListener frame (proxy [MouseAdapter] []
                             (mouseClicked
                              [e]
                              (if (pos? (bit-and MouseEvent/META_DOWN_MASK (.getModifiersEx e)))
                                (save-image canvas)
                                (.repaint canvas 0 0 0 (.getWidth canvas) (.getHeight canvas)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; oplix event handlers

(defn frame-created
  [event]
  (.setSize (oplix-frame) 300 320))

(defn opened
  [event]
  (let [fr (oplix-frame)
        cp (.getContentPane fr)
        cv (create-canvas)]
    (add-openar-banner cv)
    (.add cp cv)
    (put-oplix-prop :canvas cv)
    (add-mouse-listener fr cv))
  (set-oplix-title "About-Openar")
  (set-oplix-visible))

(defn saving
  [event]
  (let [fr (oplix-frame)
        cp (.getContentPane fr)]
    (save-dyna-listeners
     [[fr [(listener-triplet Mouse)]]])
    (.remove cp (get-oplix-prop :canvas))))

(defn saved
  [event]
  (let [fr (oplix-frame)
        cp (.getContentPane fr)]
    (.add cp (get-oplix-prop :canvas))))
