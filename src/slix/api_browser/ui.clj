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

(ns slix.api-browser.ui
  (:use [sevenri props slix ui])
  (:import (java.awt Toolkit)
           (java.awt.event WindowListener)
           (javax.swing BorderFactory JLabel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-api-browser-frame
  []
  (let [sps (slix-props)
        frm (slix-frame)
        cpn (.getContentPane frm)
        lbl (JLabel. (str (:keyword (slix-args)) " ?") JLabel/CENTER)
        fgc (create-color (read-prop sps 'foreground.color))
        bgc (create-color (read-prop sps 'background.color))]
    (doto lbl
      (.setOpaque true)
      (.setFont (create-font sps))
      (.setForeground fgc)
      (.setBackground bgc)
      (.setBorder (BorderFactory/createLineBorder bgc 4)))
    (.add cpn lbl)
    ;;
    (let [sdm (.getScreenSize (Toolkit/getDefaultToolkit))
          dim (.getPreferredSize lbl)
          wdt (.getWidth dim)
          hgt (.getHeight dim)
          cfn (bound-fn [] (invoke-later #(close-slix *slix*)))
          dtm (read-prop sps 'display.time.ms)
          woh (fn [_] (future (Thread/sleep dtm) (cfn)))]
      (doto frm
        (.setUndecorated true)
        (.setBounds (- (.getWidth sdm) wdt) (- (.getHeight sdm) hgt) wdt hgt)
        (set-listener-handlers WindowListener {'wo ['windowOpened woh]})))))
