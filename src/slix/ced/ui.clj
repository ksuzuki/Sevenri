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

(ns slix.ced.ui
  (:use [sevenri core log slix]
        [slix.ced defs])
  (:import (java.awt Color Font)
           (javax.swing BorderFactory JLabel JOptionPane PopupFactory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-font
  []
  (let [lge (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
        pref (filter #(= (ffirst *preferred-fonts*) %)
                     (map str (seq (.getAvailableFontFamilyNames lge))))
        [name style size] (if (seq pref)
                             (first *preferred-fonts*)
                             (second *preferred-fonts*))]
    (Font. name style size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; !!! Workaround for referencing protocol methods from aot-compiled lib !!!
;; This lib will be aot-compiled along with ckit when it's aot-compiled by
;; the slix Sevenri at startup and at that time reference to PProperties is
;; baked in the .class file using the compile-time created, munged class
;; name 'sevenri/props/PProperties', which is no longer available when
;; Sevenri restarts because the Clojure compiler is smart enough to not
;; recompile libs unless touched. So resolving calls to the protocol methods
;; fails as NoClassDefFound error when loading the .class file.
;; To workaround the issue, reference to sevenri.props/put-prop, for
;; example, is resolved explicitly like below. Note also that sevenri.props
;; is not :use-d in the ns form above.
(def put-prop (ns-resolve 'sevenri.props 'put-prop))

(defn ced-file-changed
  "Valid slix has to be bound to *slix*."
  [doc]
  (put-prop (slix-props) 'file (.getFile doc))
  (let [fname (.getFileName doc)
        fnlen (count fname)
        fnstr (.substring fname 0 (if (re-find #"\.clj$" fname) (- fnlen 4) fnlen))
        title (str (slix-name) " - " (path2sym fnstr))]
    (set-slix-title title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *ced-popup* (atom nil)) ;; temporary ref

;;;;

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

;;;;

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
        yna (JOptionPane/showConfirmDialog (slix-frame) msg ttl
                                           JOptionPane/YES_NO_OPTION
                                           JOptionPane/QUESTION_MESSAGE)]
    (if (= yna JOptionPane/YES_OPTION)
      (.save doc)
      ;; Save as a sid temp file and trash it so that it can be salvaged.
      (let [dtf (get-sid-temp-file (path2sym (.getFileName doc)))]
        (.setFile doc dtf)
        (.save doc)
        (trash-path? dtf)))
    (close-slix *slix*)))
