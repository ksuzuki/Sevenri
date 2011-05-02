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

(ns slix.log-viewer.ui
  (:use [sevenri props slix ui])
  (:import (java.awt Dimension Font)
           (javax.swing JScrollPane JTextArea)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [fr (slix-frame)
        cp (.getContentPane fr)
        ta (JTextArea.)
        sp (JScrollPane. ta)]
    ;;
    (doto ta
      (.setFont (Font. "Monospaced", Font/PLAIN 14))
      (.setLineWrap true)
      (.setWrapStyleWord true)
      (.setEditable false)
      (add-default-key-listener))
    ;;
    (doto sp
      (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_ALWAYS))
    (.add cp sp)
    (let [[minw minh] (read-prop (get-properties) 'slix.frame.size)
          ssize (.getScreenSize (java.awt.Toolkit/getDefaultToolkit))
          width (int (* (/ (.width ssize) 8) 5))
          hight (int (/ (.height ssize) 4))
          pos-y (- (.height ssize) hight)]
      (doto fr
        (.setMinimumSize (Dimension. minw minh))
        (.setSize width hight)
        (.setLocation 0 pos-y)))))
