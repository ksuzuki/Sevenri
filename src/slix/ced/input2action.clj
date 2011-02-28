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

(ns slix.ced.input2action
  (:use [sevenri os])
  (:import (java.awt.event InputEvent KeyEvent)
           (javax.swing KeyStroke JComponent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -map-input-to-action
  [ced]
  ;; TAB: insert-tab
  (when-let [ita (.get (.getActionMap ced) "insert-tab")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_TAB 0)]
      (.put im ky ita)))
  ;; ENTER: insert-break
  (when-let [iba (.get (.getActionMap ced) "insert-break")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_ENTER 0)]
      (.put im ky iba)))
  ;; META+]: jump-to-matching-paren-pos
  (when-let [j2opp (.get (.getActionMap ced) "jump-to-matching-paren-pos")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_CLOSE_BRACKET InputEvent/META_MASK)]
      (.put im ky j2opp)))
  ;; META+SHIFT+]: select-by-matching-paren-pos
  (when-let [sbopp (.get (.getActionMap ced) "select-by-matching-paren-pos")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_CLOSE_BRACKET (bit-or InputEvent/META_MASK InputEvent/SHIFT_MASK))]
      (.put im ky sbopp)))
  ;; META+N: switch-pane
  (when-let [sep (.get (.getActionMap ced) "switch-pane")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_N InputEvent/META_MASK)]
      (.put im ky sep)))
  ;; F1: browse-api
  (when-let [bap (.get (.getActionMap ced) "browse-api")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_F1 0)]
      (.put im ky bap)))
  ;; META+G: find-next-keyword
  (when-let [fnk (.get (.getActionMap ced) "find-next-keyword")]
    (let [im (.getInputMap ced)
          key (KeyStroke/getKeyStroke KeyEvent/VK_G InputEvent/META_MASK)]
      (.put im key fnk)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; F2: require
  (when-let [req (.get (.getActionMap ced) "require")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_F2 0)]
      (.put im ky req)))
  ;; SHIFT+F2: compile
  (when-let [com (.get (.getActionMap ced) "compile")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_F2 InputEvent/SHIFT_MASK)]
      (.put im ky com)))
  ;; F5: open-slix
  (when-let [opo (.get (.getActionMap ced) "open-slix")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_F5 0)]
      (.put im ky opo)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; META+Z: undo
  (when-let [udo (.get (.getActionMap ced) "undo")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_Z InputEvent/META_MASK)]
      (.put im ky udo)))
  ;; META+SHIFT+Z: redo
  (when-let [rdo (.get (.getActionMap ced) "redo")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_Z (bit-or InputEvent/META_MASK InputEvent/SHIFT_MASK))]
      (.put im ky rdo)))
  ;; META+S: save
  (when-let [sav (.get (.getActionMap ced) "save")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_S InputEvent/META_MASK)]
      (.put im ky sav)))
  ;; META+SHIFT+S: save-as
  (when-let [sva (.get (.getActionMap ced) "save-as")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_S (bit-or InputEvent/META_MASK InputEvent/SHIFT_MASK))]
      (.put im ky sva)))
  ;; META+O: open
  (when-let [opn (.get (.getActionMap ced) "open")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_O InputEvent/META_MASK)]
      (.put im ky opn))))

(defn- -map-input-to-action-mac
  [ced]
  ;; CTRL+R: aqua-page-up
  (when-let [apu (.get (.getActionMap ced) "aqua-page-up")]
    (let [im (.getInputMap ced)
          ky (KeyStroke/getKeyStroke KeyEvent/VK_R, InputEvent/CTRL_MASK)]
    (.put im ky apu))))

(defn map-input-to-action
  [ced]
  (-map-input-to-action ced)
  (when (is-mac?)
    (-map-input-to-action-mac ced)))
