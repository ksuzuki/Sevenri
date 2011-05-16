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
  (:import (bsh.util JConsole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -set-colors
  [console textpane]
  (let [sprps (slix-props)]
    (doto console
      (.setBackground (create-color (read-prop sprps 'console.background.color)))
      (.setPromptColor (create-color (read-prop sprps 'console.prompt.color))))
    (doto textpane
      (.setBackground (create-color (read-prop sprps 'text.background.color)))
      (.setCaretColor (create-color (read-prop sprps 'text.caret.color)))
      (.setForeground (create-color (read-prop sprps 'text.foreground.color))))))

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

(defn setup-opacity
  [frame txtpn]
  (when (enable-opacity frame)
    (add-default-opacity-key-listener frame txtpn)))

(defn initialize-ui
  []
  (let [fr (slix-frame)
        tp (-> (.getContentPane fr)
               (.getComponent 0)
               (.getViewport)
               (.getView))]
    (setup-opacity fr tp)))
