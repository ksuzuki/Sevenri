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

(ns oplix.api-browser.ui
  (:use [openar oplix ui])
  (:import (java.awt Color Font Toolkit)
           (java.awt.event WindowAdapter)
           (javax.swing BorderFactory JLabel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *fg-color* Color/black)
(def *bg-color* (Color. 255 255 204))
(def *display-time* (* 1000 3)) ;; 3sec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [frm (oplix-frame)
        cpn (.getContentPane frm)
        lbl (JLabel. (str (:keyword (oplix-args)) " ?") JLabel/CENTER)]
    (doto lbl
      (.setOpaque true)
      (.setFont (Font. "Helvetica" Font/PLAIN 14))
      (.setForeground *fg-color*)
      (.setBackground *bg-color*)
      (.setBorder (BorderFactory/createLineBorder *bg-color* 4)))
    (.add cpn lbl)
    ;;
    (let [sdm (.getScreenSize (Toolkit/getDefaultToolkit))
          dim (.getPreferredSize lbl)
          wdt (.getWidth dim)
          hgt (.getHeight dim)
          cfn (bound-fn [] (invoke-later #(close-oplix *oplix*)))]
      (doto frm
        (.setUndecorated true)
        (.setBounds (- (.getWidth sdm) wdt) (- (.getHeight sdm) hgt) wdt hgt)
        (.addWindowListener (proxy [WindowAdapter] []
                              (windowOpened
                               [e]
                               (future (Thread/sleep *display-time*)
                                       (cfn)))))))))
