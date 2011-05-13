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
  (:use [sevenri log props slix ui])
  (:import (java.awt BorderLayout Color)
           (java.awt.event KeyListener KeyEvent)
           (javax.swing.text AbstractDocument$LeafElement
                             StyleConstants$ColorConstants)
           (bsh.util JConsole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -set-colors
  [console textpane]
  (let [sprps (slix-props)
        toclr (fn [v] (if (vector? v)
                        (apply #(Color. %1 %2 %3) v)
                        (.get (.getField Color (str v)) Color)))]
    (doto console
      (.setBackground (toclr (read-prop sprps 'console.background.color)))
      (.setPromptColor (toclr (read-prop sprps 'console.prompt.color))))
    (doto textpane
      (.setBackground (toclr (read-prop sprps 'text.background.color)))
      (.setCaretColor (toclr (read-prop sprps 'text.caret.color)))
      (.setForeground (toclr (read-prop sprps 'text.foreground.color))))))

(defn create-content-pane
  [cp]
  (let [jc (JConsole.)
        tp (.getTextPane jc)]
    (-set-colors jc tp)
    (add-default-key-listener tp)
    (.add cp jc)))

(defn create-repl-frame
  []
  (let [frame (slix-frame)
        [w h] (read-prop (slix-props) 'frame.size)]
    (create-content-pane (.getContentPane frame))
    (doto frame
      (.pack)
      (.setSize w h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -setup-opacity
  "LEFT/RIGHT keys increase/decrease opacity.
   UP/DOWN keys let it cycle between current-opacity, 1.0, and 0.1."
  [frame txtpn]
  (when (is-translucent-supported?)
    (let [sp (slix-props (get-slix frame))
          ov (or (.getClientProperty txtpn 'opacity) (read-prop sp 'opacity.default))]
      (set-window-opacity frame (float ov))
      (doto txtpn
        (.putClientProperty 'opacity (int (* ov 100)))
        (.putClientProperty 'opacity.delta (read-prop sp 'opacity.delta))))
    ;;
    (set-listener-handlers txtpn
     KeyListener
     {'kp ['keyPressed (fn [e] 
                         (let [kc (.getKeyCode e)
                               mo (.getModifiers e)
                               mk (bit-or KeyEvent/ALT_MASK KeyEvent/META_MASK)]
                           (when (and (or (= kc KeyEvent/VK_LEFT) (= kc KeyEvent/VK_RIGHT)
                                          (= kc KeyEvent/VK_UP) (= kc KeyEvent/VK_DOWN))
                                      (= mk (bit-and mo mk)))
                             (let [ovo (get-window-opacity frame)
                                   opc (.getClientProperty txtpn 'opacity)
                                   dlt (.getClientProperty txtpn 'opacity.delta)
                                   ovn (cond
                                        (= kc KeyEvent/VK_LEFT)  (min 1.0 (- ovo dlt))
                                        (= kc KeyEvent/VK_RIGHT) (max 0.1 (+ ovo dlt))
                                        (= kc KeyEvent/VK_UP) (if (<= 1.0 ovo)
                                                                0.1
                                                                (if (= (int (* ovo 100)) opc)
                                                                  1.0
                                                                  (/ opc 100.0)))
                                        (= kc KeyEvent/VK_DOWN) (if (<= ovo 0.1)
                                                                  1.0
                                                                  (if (= (int (* ovo 100)) opc)
                                                                    0.1
                                                                    (/ opc 100.0)))
                                        :else -1.0)]
                               #_(lg "ovo:" ovo "ovn:" ovn "opacity:" opc)
                               (when (pos? ovn)
                                 (set-window-opacity frame (float ovn))
                                 (when (or (= kc KeyEvent/VK_LEFT) (= kc KeyEvent/VK_RIGHT))
                                   (.putClientProperty txtpn 'opacity (int (* ovn 100)))))))))]})))

(defn ui-initialize
  []
  (let [fr (slix-frame)
        cp (.getContentPane fr)
        jc (.getComponent cp 0)
        tp (.getComponent (.getViewport jc) 0)]
    (-setup-opacity fr tp)))
