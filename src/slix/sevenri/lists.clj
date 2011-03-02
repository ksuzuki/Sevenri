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

(ns slix.sevenri.lists
  (:use [sevenri config core log slix utils]
        [slix.sevenri defs]
        [clojure.java io])
  (:import (java.awt Cursor)
           (java.awt.event InputEvent MouseAdapter)
           (javax.swing.event ListSelectionListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-main-panel-components
  [frame]
  (let [mp (.getComponent (.getContentPane frame) 0)]
    {:mainPanel mp
     :lblSevenri (.lblSevenri mp)
     :spDivider (.spDivider mp) :lstSn (.lstSn mp) :lstName (.lstName mp)}))

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

(defn get-slix-names-by-sns
  ([sns]
     (get-slix-names-by-sns sns nil))
  ([sns names]
     (if (seq sns)
       (let [fs (filter #(= (str (first sns)) (str (slix-sn %))) (get-slixes))]
         (recur (rest sns) (if (seq fs)
                             (concat (map #(str (slix-name %)) fs) names)
                             names)))
       (when (seq names)
         (sort names)))))

(defn get-registered-sns-by-names
  ([nms]
     (get-registered-sns-by-names nms nil))
  ([nms sns]
     (if (seq nms)
       (let [fs (filter #(= (str (first nms)) (str (slix-name %))) (get-slixes))]
         (recur (rest nms) (if (seq fs)
                             (cons (str (slix-sn (first fs))) sns)
                             sns)))
         (when (seq sns)
           (sort sns)))))

(defn get-slix-frame-titles-by-names
  ([nms]
     (get-slix-frame-titles-by-names nms nil))
  ([nms titles]
     (if (seq nms)
       (let [fs (filter #(= (str (first nms)) (str (slix-name %))) (get-slixes))]
         (recur (rest nms) (if (seq fs)
                             (let [s (first fs)]
                               (cons (slix-name s) (cons (.getTitle (slix-frame s)) titles)))
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

(defn handle-sn-list-event*
  [mpcs lstSn]
  (let [sns (seq (.getSelectedValues lstSn))
        lstName (:lstName mpcs)
        lsls (seq (.getListSelectionListeners lstName))]
    ;; Remove lstName listeners temporarily to prevent valueChanged event loop.
    (doseq [lsl lsls]
      (.removeListSelectionListener lstName lsl))
    ;; Update the list.
    (if-let [names (get-slix-names-by-sns sns)]
      (let [name-title-map (get-slix-frame-titles-by-names names)
            name-titles (map #(str (first %) " \"" (second %) "\"") name-title-map)
            title-name-map (apply hash-map (interleave name-titles (keys name-title-map)))
            sis (get-selected-indices (seq (.getSelectedValues lstName)) name-titles)]
        (doto lstName
          (.setListData (into-array String name-titles))
          (.setSelectedIndices (int-array sis))
          (.putClientProperty *prop-title-name-map* title-name-map)))
      (.setListData lstName (into-array String [])))
    ;; Restore listeners.
    (doseq [lsl lsls]
      (.addListSelectionListener lstName lsl))))

(defn handle-sn-list-event
  [e]
  (when-not (.getValueIsAdjusting e)
    (when-let [frame (.getTopLevelAncestor (.getSource e))]
      (let [mpcs (get-main-panel-components frame)
            lstSn (:lstSn mpcs)]
        (handle-sn-list-event* mpcs lstSn)))))

(defn get-sn-list-listener
  []
  (proxy [ListSelectionListener] []
    (valueChanged [e] (handle-sn-list-event e))))

(defn get-sn-list-mouse-listener
  []
  (proxy [MouseAdapter] []
    (mouseClicked
     [e]
     (let [cc (.getClickCount e)
           lstSn (.getComponent e)
           frame (.getTopLevelAncestor lstSn)]
       (cond
        (= cc 1) (let [mpcs (get-main-panel-components frame)]
                   (handle-sn-list-event* mpcs lstSn))
        (= cc 2) (when-not (.isSelectionEmpty lstSn)
                   (let [sn (symbol (.getSelectedValue lstSn))
                         open-lib? (pos? (bit-and (.getModifiersEx e) InputEvent/META_DOWN_MASK))
                         alt-open? (pos? (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK))]
                       (future
                         (let [oc (.getCursor frame)]
                           (.setCursor frame Cursor/WAIT_CURSOR)
                           (try
                             (deref
                              (cond
                               open-lib? (open-slix-with-args {:file (get-slix-fqns sn)} 'ced)
                               alt-open? (open-slix-with-args {(get-default :slix :arguments :alt-open) true} sn)
                               :else (open-slix sn)))
                             (finally
                              (.setCursor frame oc)))))))
        :else nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-name-list-event*
  [mpcs lstName]
  (let [title-name-map (.getClientProperty lstName *prop-title-name-map*)
        sns-of-nms (reduce (fn [s o] (conj s o))
                           #{}
                           (map #(slix-sn (get-slix %)) (vals title-name-map)))
        name-titles (.getSelectedValues lstName)
        nms (map #(get title-name-map %) name-titles)
        sns-by-nms (get-registered-sns-by-names nms)
        ;;
        lstSn (:lstSn mpcs)
        lm (.getModel lstSn)
        sns-listed (map #(.getElementAt lm %) (range (.getSize lm)))
        ;;
        snlsls (seq (.getListSelectionListeners lstSn))
        nmlsls (seq (.getListSelectionListeners lstName))]
    ;; Remove listeners temporarily to prevent valueChanged event loop.
    (doseq [lsl snlsls]
      (.removeListSelectionListener lstSn lsl))
    (doseq [lsl nmlsls]
      (.removeListSelectionListener lstName lsl))
    ;; Update lists only when there are multiples sns.
    (when (< 1 (count sns-of-nms))
      (if-let [indecies (get-item-indices sns-listed sns-by-nms)]
        (do
          (.setListData lstName (into-array String name-titles))
          (.setSelectedIndices lstName (int-array (range (count name-titles))))
          (.setSelectedIndices lstSn (int-array indecies)))
        (.setListData lstName (into-array String []))))
    ;; Restore listeners.
    (doseq [lsl nmlsls]
      (.addListSelectionListener lstName lsl))
    (doseq [lsl snlsls]
      (.addListSelectionListener lstSn lsl))))

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
                       (.toFront (slix-frame (get-slix nm))))))
        :else nil)))))
