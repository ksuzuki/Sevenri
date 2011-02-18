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

(ns oplix.repl.listeners
  (:use [openar log oplix]
        [oplix.repl defs]
        [library.oplix.ced paren])
  (:import (java.awt Color EventQueue)
           (java.awt.event KeyAdapter KeyEvent)
           (javax.swing.event CaretListener)
           (javax.swing.text Position SimpleAttributeSet StyleConstants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-highlighting
  [txtpn doc]
  (when-let [ppp (.getClientProperty txtpn *ppp-info*)]
    (let [ppp [(.getOffset (aget ppp 0)) (.getOffset (aget ppp 1))]
          atr (SimpleAttributeSet.)
          len (.getLength doc)]
      (doto atr
        (StyleConstants/setForeground (.getForeground txtpn))
        (StyleConstants/setBackground (.getBackground txtpn)))
      (EventQueue/invokeLater
       #(doseq [pp ppp]
          (when (< pp len)
            (.setCharacterAttributes doc pp 1 atr true))))
      (EventQueue/invokeLater
       #(.setCharacterAttributes txtpn (.getCharacterAttributes txtpn) true)))))

(defn highlight-matching-paren
  [txtpn doc cpos spos]
  (let [[ppp fmpparam] (find-matching-paren txtpn cpos {:min-pos spos})]
    (when ppp
      (let [ppi [(.createPosition doc (first ppp)) (.createPosition doc (second ppp))]
            atr (SimpleAttributeSet.)]
        (.putClientProperty txtpn *ppp-info* (into-array ppi))
        (doto atr
          (StyleConstants/setForeground (.getSelectedTextColor txtpn))
          (StyleConstants/setBackground (.getSelectionColor txtpn)))
        (EventQueue/invokeLater
         #(doseq [pp ppp]
            (.setCharacterAttributes doc pp 1 atr true)))))))

(defn restart-repl
  [oplix start-repl]
  (when-let [repl-thread (get-oplix-prop oplix :repl-thread)]
    (let [con (get-oplix-prop oplix :console)
          exc (Exception. "^C")]
      (.resetIO con)
      (.stop repl-thread exc)
      (start-repl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-listeners
  [txtpn doc con start-repl oplix]
  (.setPPPInfoKey con *ppp-info*)
  (.addCaretListener
   txtpn
   (proxy [CaretListener] []
     (caretUpdate [e]
       (clear-highlighting txtpn doc)
       (let [cpos (.getDot e)
             spos (.getPromptStartPosition con)]
         (if (<= spos cpos)
           (highlight-matching-paren txtpn doc cpos spos)
           (.putClientProperty txtpn *ppp-info* nil))))))
  (.addKeyListener
   txtpn
   (proxy [KeyAdapter] []
     (keyPressed [e]
       (let [kc (.getKeyCode e)]
         (when (and (= kc KeyEvent/VK_C) (pos? (bit-and (.getModifiers e) KeyEvent/CTRL_MASK)))
           (restart-repl oplix start-repl)))))))
