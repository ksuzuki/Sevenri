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

(ns oplix.openar.lists
  (:use [openar config core log oplix utils]
        [oplix.openar defs]
        [clojure.java io])
  (:import (java.awt Cursor)
           (java.awt.event InputEvent MouseAdapter)
           (javax.swing.event ListSelectionListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-main-panel-components
  [frame]
  (let [mp (.getComponent (.getContentPane frame) 0)]
    {:mainPanel mp
     :lblOpenar (.lblOpenar mp)
     :spDivider (.spDivider mp) :lstOn (.lstOn mp) :lstName (.lstName mp)}))

(defn get-selected-indices
  [old-selections new-selections]
  (if (seq old-selections)
    (let [sis (loop [sis []
                     idx 0
                     nss new-selections]
                (if (seq nss)
                  (recur (if (seq (filter #(= (first nss) %) old-selections))
                           (conj sis idx)
                           sis)
                         (inc idx)
                         (rest nss))
                  sis))]
      (if (seq sis)
        sis
        [0]))
    [0]))

(defn get-oplix-names-by-ons
  ([ons]
     (get-oplix-names-by-ons ons nil))
  ([ons names]
     (if (seq ons)
       (let [fo (filter #(= (str (first ons)) (str (oplix-on %))) (get-oplixes))]
         (recur (rest ons) (if (seq fo)
                             (concat (map #(str (oplix-name %)) fo) names)
                             names)))
       (when (seq names)
         (sort names)))))

(defn get-registered-ons-by-names
  ([nms]
     (get-registered-ons-by-names nms nil))
  ([nms ons]
     (if (seq nms)
       (let [fo (filter #(= (str (first nms)) (str (oplix-name %))) (get-oplixes))]
         (recur (rest nms) (if (seq fo)
                             (cons (str (oplix-on (first fo))) ons)
                             ons)))
         (when (seq ons)
           (sort ons)))))

(defn get-oplix-frame-titles-by-names
  ([nms]
     (get-oplix-frame-titles-by-names nms nil))
  ([nms titles]
     (if (seq nms)
       (let [fo (filter #(= (str (first nms)) (str (oplix-name %))) (get-oplixes))]
         (recur (rest nms) (if (seq fo)
                             (let [o (first fo)]
                               (cons (oplix-name o) (cons (.getTitle (oplix-frame o)) titles)))
                             titles)))
       (when (seq titles)
         (apply sorted-map titles)))))

(defn get-item-indices
  ([items selections]
     (get-item-indices (map #(list %1 %2) items (range)) selections nil))
  ([indexed-items selections indices]
     (if (seq selections)
       (recur indexed-items (rest selections)
              (let [f (filter (fn [[item _]] (= item (first selections))) indexed-items)]
                (if (seq f)
                  (conj indices (second (first f)))
                  indices)))
       (when (seq indices)
         indices))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-on-list-event*
  [mpcs lstOn]
  (let [ons (seq (.getSelectedValues lstOn))
        lstName (:lstName mpcs)
        lsls (seq (.getListSelectionListeners lstName))]
    ;; Remove lstName listeners temporarily to prevent valueChanged event loop.
    (doseq [lsl lsls]
      (.removeListSelectionListener lstName lsl))
    ;; Update the list.
    (if-let [names (get-oplix-names-by-ons ons)]
      (let [name-title-map (get-oplix-frame-titles-by-names names)
            name-titles (map #(str (first %) " \"" (second %) "\"") name-title-map)
            title-name-map (apply hash-map (interleave name-titles (keys name-title-map)))
            sis (get-selected-indices (seq (.getSelectedValues lstName)) name-titles)]
        (doto lstName
          (.setListData (into-array String name-titles))
          (.setSelectedIndices (into-array Integer/TYPE sis))
          (.putClientProperty *prop-title-name-map* title-name-map)))
      (.setListData lstName (into-array String [])))
    ;; Restore listeners.
    (doseq [lsl lsls]
      (.addListSelectionListener lstName lsl))))

(defn handle-on-list-event
  [e]
  (when-not (.getValueIsAdjusting e)
    (when-let [frame (.getTopLevelAncestor (.getSource e))]
      (let [mpcs (get-main-panel-components frame)
            lstOn (:lstOn mpcs)]
        (handle-on-list-event* mpcs lstOn)))))

(defn get-on-list-listener
  []
  (proxy [ListSelectionListener] []
    (valueChanged [e] (handle-on-list-event e))))

(defn get-on-list-mouse-listener
  []
  (proxy [MouseAdapter] []
    (mouseClicked
     [e]
     (let [cc (.getClickCount e)
           lstOn (.getComponent e)
           frame (.getTopLevelAncestor lstOn)]
       (cond
        (= cc 1) (let [mpcs (get-main-panel-components frame)]
                   (handle-on-list-event* mpcs lstOn))
        (= cc 2) (when-not (.isSelectionEmpty lstOn)
                   (let [on (symbol (.getSelectedValue lstOn))
                         open-lib? (pos? (bit-and (.getModifiersEx e) InputEvent/META_DOWN_MASK))
                         alt-open? (pos? (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK))]
                       (future
                         (let [oc (.getCursor frame)]
                           (.setCursor frame Cursor/WAIT_CURSOR)
                           (try
                             (deref
                              (cond
                               open-lib? (open-oplix-with-args {:file (get-oplix-fqns on)} 'ced)
                               alt-open? (open-oplix-with-args {(get-default :oplix :arguments :alt-open) true} on)
                               :else (open-oplix on)))
                             (finally
                              (.setCursor frame oc)))))))
        :else nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-name-list-event*
  [mpcs lstName]
  (let [title-name-map (.getClientProperty lstName *prop-title-name-map*)
        ons-of-nms (reduce (fn [s o] (conj s o))
                           #{}
                           (map #(oplix-on (get-oplix %)) (vals title-name-map)))
        name-titles (.getSelectedValues lstName)
        nms (map #(get title-name-map %) name-titles)
        ons-by-nms (get-registered-ons-by-names nms)
        ;;
        lstOn (:lstOn mpcs)
        lm (.getModel lstOn)
        ons-listed (map #(.getElementAt lm %) (range (.getSize lm)))
        ;;
        onlsls (seq (.getListSelectionListeners lstOn))
        nmlsls (seq (.getListSelectionListeners lstName))]
    ;; Remove listeners temporarily to prevent valueChanged event loop.
    (doseq [lsl onlsls]
      (.removeListSelectionListener lstOn lsl))
    (doseq [lsl nmlsls]
      (.removeListSelectionListener lstName lsl))
    ;; Update lists only when there are multiples ons.
    (when (< 1 (count ons-of-nms))
      (if-let [indecies (get-item-indices ons-listed ons-by-nms)]
        (do
          (.setListData lstName (into-array String name-titles))
          (.setSelectedIndices lstName (into-array Integer/TYPE (range (count name-titles))))
          (.setSelectedIndices lstOn (into-array Integer/TYPE indecies)))
        (.setListData lstName (into-array String []))))
    ;; Restore listeners.
    (doseq [lsl nmlsls]
      (.addListSelectionListener lstName lsl))
    (doseq [lsl onlsls]
      (.addListSelectionListener lstOn lsl))))

(defn handle-name-list-event
  [e]
  (when-not (.getValueIsAdjusting e)
    (when-let [frame (.getTopLevelAncestor (.getSource e))]
      (let [mpcs (get-main-panel-components frame)
            lstName (:lstName mpcs)]
        (handle-name-list-event* mpcs lstName)))))

(defn get-name-list-listener
  []
  (proxy [ListSelectionListener] []
    (valueChanged [e] (handle-name-list-event e))))

(defn get-name-list-mouse-listener
  []
  (proxy [MouseAdapter] []
    (mouseClicked
     [e]
     (let [cc (.getClickCount e)
           lstName (.getComponent e)]
       (cond
        (= cc 1) (let [frame (.getTopLevelAncestor lstName)
                       mpcs (get-main-panel-components frame)]
                   (handle-name-list-event* mpcs lstName))
        (= cc 2) (let [title-name-map (.getClientProperty lstName *prop-title-name-map*)]
                   (when-not (.isSelectionEmpty lstName)
                     (let [nm (get title-name-map (.getSelectedValue lstName))]
                       (.toFront (oplix-frame (get-oplix nm))))))
        :else nil)))))
