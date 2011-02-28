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
