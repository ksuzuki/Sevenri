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

(ns slix.documenter.keymap
  (:use [sevenri log slix]
        [slix.documenter io ui])
  (:import (java.awt.event InputEvent KeyEvent)
           (javax.swing JTextPane KeyStroke PopupFactory)
           (javax.swing.text TextAction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *documenter-keymap* "documenter-keymap")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-save-section-text-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_S InputEvent/META_MASK)
        ac (proxy [TextAction] ["save-section-text"]
             (actionPerformed [e]
               (let [sectx (:section-text controls)
                     frame (.getTopLevelAncestor sectx)]
                 (write-section sectx)
                 (popup-save-message frame))))]
    [ks ac]))

(defn get-publish-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_P InputEvent/META_MASK)
        ac (proxy [TextAction] ["publish"]
             (actionPerformed [e]
               (.doClick (:publish controls))))]
    [ks ac]))

(defn get-review-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_R InputEvent/META_MASK)
        ac (proxy [TextAction] ["review"]
             (actionPerformed [e]
               (.doClick (:review controls))))]
    [ks ac]))

(defn get-page-up-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_R InputEvent/CTRL_MASK)
        ac (.get (.getActionMap (:section-text controls)) "page-up")]
    [ks ac]))

(defn get-page-down-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_V InputEvent/CTRL_MASK)
        ac (.get (.getActionMap (:section-text controls)) "page-down")]
    [ks ac]))

(defn get-find-next-in-section-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_G InputEvent/META_MASK)
        ac (proxy [TextAction] ["find-next"]
             (actionPerformed [e]
               (find-next-in-section (:section-text controls))))]
    [ks ac]))

(defn get-find-in-section-key-action-pair
  [controls]
  (let [ks (KeyStroke/getKeyStroke KeyEvent/VK_F InputEvent/META_MASK)
        ac (proxy [TextAction] ["find"]
             (actionPerformed [e]
               (find-in-section (:section-text controls))))]
    [ks ac]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-key-action-pairs
  [controls]
  [(get-save-section-text-key-action-pair controls)
   (get-publish-key-action-pair controls)
   (get-review-key-action-pair controls)
   (get-page-up-key-action-pair controls)
   (get-page-down-key-action-pair controls)
   (get-find-next-in-section-key-action-pair controls)
   (get-find-in-section-key-action-pair controls)])

(defn add-bindings
  [keymap controls]
  (reduce (fn [km [ks ac]]
            (.addActionForKeyStroke km ks ac)
            km)
          keymap
          (get-key-action-pairs controls)))

(defn add-keymap-to-editor
  [frame]
  (let [ctrls (get-controls frame)
        sectx (:section-text ctrls)]
    (if-let [dwkm (JTextPane/getKeymap *documenter-keymap*)]
      (.setKeymap sectx dwkm)
      (let [tpkm (.getKeymap sectx)
            dwkm (JTextPane/addKeymap *documenter-keymap* tpkm)]
        (add-bindings dwkm ctrls)
        (.setKeymap sectx dwkm)))))

(defn reset-editor-keymap
  [frame]
  (let [ctrls (get-controls frame)
        sectx (:section-text ctrls)
        dfkmp (JTextPane/getKeymap JTextPane/DEFAULT_KEYMAP)]
    (.setKeymap sectx dfkmp)))
