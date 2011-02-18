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

(ns oplix.openar.drawicon
  (:use [openar log])
  (:import (java.awt BasicStroke BorderLayout Color GradientPaint RenderingHints)
           (java.awt.geom AffineTransform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-gradient-paint
  [w h angle]
  (let [r (* 4/360 (Math/toDegrees angle))]
    (GradientPaint. 0 0
                    (cond
                     (<= r 0.3) (Color. 255 32 32 160)
                     (<= r 0.6) (Color. 32 255 32 160)
                     (<= r 0.9) (Color. 32 32 255 160)
                     :else (Color. 255 255 255 160))
                    w h
                    (Color. (int (* Integer/MAX_VALUE r))))))

(defn draw
  ([canvas g]
     (draw canvas g false))
  ([canvas g black-bg?]
     (let [cw (.getWidth canvas) ch (.getHeight canvas)]
       (.setRenderingHint g
                          RenderingHints/KEY_ANTIALIASING
                          RenderingHints/VALUE_ANTIALIAS_ON)
       ;;
       (when black-bg?
         (doto g
           (.setColor Color/black)
           (.fillRect 0 0 cw ch)))
       ;;
       (.setTransform g (AffineTransform. 1.0 0.0 0.0 -1.0
                                          (double (* (/ cw 20) 4))
                                          (double (* (/ ch 20) 16))))
       (let [ow (* (/ cw 20) 15) oh (/ ow 3)
             t (.getTransform g)]
         (.setStroke g (BasicStroke. 5))
         (doseq [a (range 0 (/ Math/PI 2) (/ Math/PI 12))]
           (doto g
             (.rotate a)
             (.setPaint (get-gradient-paint cw ch a))
             (.fillOval 0 (- (/ oh 2)) ow oh)
             (.setTransform t)))))))
