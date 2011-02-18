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

(ns oplix.ced.ui
  (:use [openar core log oplix]
        [oplix.ced defs])
  (:import (java.awt Color Font)
           (javax.swing BorderFactory JLabel JOptionPane PopupFactory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *ced-popup* (atom nil)) ;; temporary ref

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ced-popup
  []
  @*ced-popup*)

(defn save-ced-popup
  [popup]
  (when-let [cp (get-ced-popup)]
    (.hide cp))
  (reset! *ced-popup* popup))

(defn clear-ced-popup
  []
  (when-let [cp (get-ced-popup)]
    (.hide cp))
  (reset! *ced-popup* nil))

(defn create-ced-popup
  [owner x y font msg fgc bgc]
  (let [lbl (JLabel. "" JLabel/CENTER)
        pup (.getPopup (PopupFactory/getSharedInstance) owner lbl x y)]
    (doto lbl
      (.setOpaque true)
      (.setFont font)
      (.setText (str " " msg " "))
      (.setBorder (BorderFactory/createLineBorder bgc 2))
      (.setForeground fgc)
      (.setBackground bgc))
    pup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-ced-file-popup
  [frame doc pos]
  (let [loc (.getLocationOnScreen frame)
        lcx (+ (.x loc) (.x pos) 8)
        lcy (+ (.y loc) (.y pos) -8)]
    (create-ced-popup frame lcx lcy
                      (Font. "Helvetica" Font/PLAIN 14) (.getFile doc)
                      Color/black *popup-bg-color*)))

(defn popup-warning
  [frame msg]
  (let [loc (.getLocationOnScreen frame)
        pup (create-ced-popup frame (.x loc) (.y loc)
                              (Font. "Helvetica" Font/PLAIN 14) msg
                              Color/red *popup-bg-color*)
        agt (agent pup)
        afn (fn [p]
              (.show p)
              (Thread/sleep (* 1000 *warning-sec*))
              (.hide p))]
    (send agt afn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ask-then-close
  [doc]
  (let [msg "Save before close?"
        ttl "Save Before Close"
        yna (JOptionPane/showConfirmDialog (oplix-frame) msg ttl
                                           JOptionPane/YES_NO_OPTION
                                           JOptionPane/QUESTION_MESSAGE)]
    (if (= yna JOptionPane/YES_OPTION)
      (.save doc)
      ;; Save as a dop temp file and trash it so that it can be salvaged.
      (let [dtf (get-dop-temp-file (.getFileName doc))]
        (.setFile doc dtf)
        (.save doc)
        (trash-file? dtf)))
    (close-oplix *oplix*)))
