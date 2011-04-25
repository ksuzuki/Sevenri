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

(ns slix.ced.cview
  (:gen-class
   :extends javax.swing.text.PlainView
   :exposes-methods {drawUnselectedText superDrawUnselectedText}
   :main false)
  (:use [sevenri log]
        [slix.ced defs])
  (:import (javax.swing.text Segment Utilities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn drawParenChar
  "Draw a paren char in highlight. Note that when rendering text using
   Utilities/drawTabbedText, a color has to be set prior to call the fn,
   or you get blank."
  [this g x y p]
  (let [seg (Segment.)
        fmx (.getFontMetrics g)
        ast (.getAscent fmx)
        hgt (+ ast (.getDescent fmx))]
    (.getText (.getDocument this) p 1 seg)
    (let [px (Utilities/drawTabbedText seg x y g this 0)]
      ;; Fill the paren char in the reverse color of the foreground.
      (.setXORMode g (.getBackground g))
      (.fillRect g x (- y ast) (- px x) hgt)
      (.setPaintMode g)
      px)))

(defn drawParenText
  "Repeat drawing unselected text and paren char."
  [this g x y p0 p1 pp0 pp1]
  ;; Don't mess the original graphics context. Working with a copy and
  ;; disposing it in the end is the Swing way.
  (let [pcg (.create g)]
    ;; Use g to draw text and use pcg to draw paren char in
    ;; *paren-highlight-color*. Use the try form to ensure disposing pcg.
    (try
      (.setColor pcg *paren-highlight-color*)
      (loop [x x
             a p0
             p p0]
        (if (< p p1)
          (if (or (= p pp0) (= p pp1))
            (let [tx (if (< a p) (.superDrawUnselectedText this g x y a p) x)
                  px (drawParenChar this pcg tx y p)
                  np (inc p)]
              (recur px np np))
            (recur x a (inc p)))
          ;; Draw rest of the unselected text if any.
          (when (< a p)
            ;; The return value of this form is going to be returned to the
            ;; caller because this is the last call in the try form.
            (.superDrawUnselectedText this g x y a p))))
      (finally
       (.dispose pcg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -drawUnselectedText
  [this g x y p0 p1]
  "The rendering range is actually [p0, p1)."
  (let [ced (.getContainer this)
        ppp (.getClientProperty ced *prop-ced-ppp-info*)
        [pp0 pp1] (if (and ppp (not (second ppp)))
                    (or (first ppp) [Long/MAX_VALUE -1])
                    [Long/MAX_VALUE -1])]
    (if (or (<= p1 pp0) ;; The range is before the begin-paren.
            (< pp1 p0)  ;; The range is after the end-paren.
            (and (< pp0 p0) (<= p1 pp1))) ;; The range is inside the parens exclusively.
      (.superDrawUnselectedText this g x y p0 p1)
      ;; The range includes either or both the begin- and end-paren.
      (drawParenText this g x y p0 p1 pp0 pp1))))
