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

(ns slix.repl.ui
  (:use [sevenri log slix ui]
        [slix.repl defs])
  (:import (java.awt BorderLayout Color)
           (java.awt.event KeyAdapter KeyEvent)
           (javax.swing.text AbstractDocument$LeafElement
                             StyleConstants$ColorConstants)
           (bsh.util JConsole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -set-colors
  [jconsole]
  (doto jconsole
    (.setBackground *console-background-color*)
    (.setPromptColor *console-prompt-color*))
  (doto (.getTextPane jconsole)
    (.setForeground *text-foreground-color*)
    (.setBackground *text-background-color*)
    (.setCaretColor *text-caret-color*)))

(defn create-content-pane
  [cp]
  (let [jc (JConsole.)
        tp (.getComponent (.getViewport jc) 0)]
    (assert jc)
    (-set-colors jc)
    ;; Close with META+W.
    (add-default-key-listener tp)
    (doto cp
      (.setLayout (BorderLayout.))
      (.add jc))))

(defn create-frame
  [frame]
  (doto frame
    (.pack)
    (.setSize 640 480)))

(defn create-repl-frame
  []
  (let [fr (slix-frame)
        cp (.getContentPane fr)]
    (create-content-pane cp)
    (create-frame fr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -set-opacity
  "Start from opacity 0.9.
   LEFT/RIGHT keys increase/decrease opacity.
   UP/DOWN keys let it cycle between current-opacity, 1.0, and 0.1."
  [frame txtpn]
  (when (is-translucent-supported?)
    (let [ov (or (.getClientProperty txtpn *prop-opacity*) *opacity-default*)]
      (set-window-opacity frame (float ov))
      (.putClientProperty txtpn *prop-opacity* (int (* ov 100))))
    ;;
    (.addKeyListener
     txtpn
     (proxy [KeyAdapter] []
       (keyPressed [e]
         (let [kc (.getKeyCode e)
               mo (.getModifiers e)
               mk (bit-or KeyEvent/ALT_MASK KeyEvent/META_MASK)]
           (when (and (or (= kc KeyEvent/VK_LEFT) (= kc KeyEvent/VK_RIGHT)
                          (= kc KeyEvent/VK_UP) (= kc KeyEvent/VK_DOWN))
                      (= mk (bit-and mo mk)))
             (let [ovo (get-window-opacity frame)
                   ovn (cond
                        (= kc KeyEvent/VK_LEFT) (min 1.0 (- ovo *opacity-delta*))
                        (= kc KeyEvent/VK_RIGHT) (max 0.1 (+ ovo *opacity-delta*))
                        (= kc KeyEvent/VK_UP) (if (<= 1.0 ovo)
                                                0.1
                                                (if (= (int (* ovo 100))
                                                       (.getClientProperty txtpn *prop-opacity*))
                                                  1.0
                                                  (/ (.getClientProperty txtpn *prop-opacity*) 100.0)))
                        (= kc KeyEvent/VK_DOWN) (if (<= ovo 0.1)
                                                  1.0
                                                  (if (= (int (* ovo 100))
                                                         (.getClientProperty txtpn *prop-opacity*))
                                                    0.1
                                                    (/ (.getClientProperty txtpn *prop-opacity*) 100.0)))
                        :else -1.0)]
               #_(lg "ovo:" ovo "ovn:" ovn "*prop-opacity*:" (.getClientProperty txtpn *prop-opacity*))
               (when (pos? ovn)
                 (set-window-opacity frame (float ovn))
                 (when (or (= kc KeyEvent/VK_LEFT) (= kc KeyEvent/VK_RIGHT))
                   (.putClientProperty txtpn *prop-opacity* (int (* ovn 100)))))))))))))

(defn customize-repl-frame
  []
  (let [fr (slix-frame)
        cp (.getContentPane fr)
        jc (.getComponent cp 0)
        tp (.getComponent (.getViewport jc) 0)]
    (-set-opacity fr tp)))
