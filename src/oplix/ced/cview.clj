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

(ns oplix.ced.cview
  (:gen-class
   :extends javax.swing.text.PlainView
   :exposes-methods {drawUnselectedText superDrawUnselectedText}
   :main false)
  (:use [openar log]
        [oplix.ced defs])
  (:import (javax.swing.text Segment Utilities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn drawTextSegment
  [this g x y doc clr seg a p]
  (.setColor g clr)
  (.getText doc a (- p a) seg)
  (Utilities/drawTabbedText seg x y g this a))

(defn drawParenText
  [this g x y p0 p1 doc fgc seg pp0 pp1]
  (let [fm (.getFontMetrics g)
        as (.getAscent fm)
        ht (+ as (.getDescent fm))
        bgc (.getBackground g)]
    (loop [x x
           a p0
           p p0]
      (if (< p p1)
        (if (or (= p pp0) (= p pp1))
          (let [nextp (inc p)
                txbeg (drawTextSegment this g x y doc fgc seg a p)
                txend (drawTextSegment this g txbeg y doc *paren-highlight-color* seg p nextp)]
            (.setColor g *paren-highlight-color*)
            (.setXORMode g bgc)
            (.fillRect g txbeg (- y as) (- txend txbeg) ht)
            (.setPaintMode g)
            (recur txend nextp nextp))
          (recur x a (inc p)))
        (drawTextSegment this g x y doc fgc seg a p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -drawUnselectedText
  [this g x y p0 p1]
  "The rendering pos range is actually [p0, p1). Also when rendering using
   Utilities/drawTabbedText, a color has to be set prior to call the fn,
   or you get blank."
  (let [doc (.getDocument this)
        ced (.getContainer this)
        fgc (.getForeground ced)
        seg (Segment.)
        ppp (.getClientProperty ced *prop-ced-ppp-info*)
        [pp0 pp1] (if (and ppp (not (second ppp)))
                    (or (first ppp) [Long/MAX_VALUE -1])
                    [Long/MAX_VALUE -1])]
    (if (or (< pp1 p0) (<= p1 pp0))
      (drawTextSegment this g x y doc fgc seg p0 p1)
      (drawParenText this g x y p0 p1 doc fgc seg pp0 pp1))))
