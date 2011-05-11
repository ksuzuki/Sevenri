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
  (:use [sevenri core log props slix ui]
        [slix.sevenri ui]
        [clojure.java io])
  (:import (java.awt Cursor)
           (java.awt.event InputEvent MouseListener)
           (javax.swing.event ListSelectionListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn handle-list-sn-event*
  [list-sn frame]
  (let [fprps (frame-props frame)]
    (when-not (get-prop fprps 'list.updating)
      ;; Prevent recursive valueChanged event loop 
      (put-prop fprps 'list.updating true)
      ;; Start updating lists.
      (let [mpcs (get-main-panel-components frame)
            sns (seq (.getSelectedValues list-sn))
            list-name (:list-name mpcs)
            lsls (seq (.getListSelectionListeners list-name))]
        (if-let [names (get-slix-names-by-sns sns)]
          (let [name-title-map (get-slix-frame-titles-by-names names)
                name-titles (map #(str (first %) " \"" (second %) "\"") name-title-map)
                title-name-map (apply hash-map (interleave name-titles (keys name-title-map)))
                sis (get-selected-indices (seq (.getSelectedValues list-name)) name-titles)]
            (doto list-name
              (.setListData (into-array String name-titles))
              (.setSelectedIndices (into-array Integer/TYPE sis))
              (.putClientProperty "title-name-map" title-name-map)))
          (.setListData list-name (into-array String [])))
        ;; Done.
        (put-prop fprps 'list.updating false)))))

(defn handle-list-sn-value-changed
  [e]
  (when-not (.getValueIsAdjusting e)
    (let [list-sn (.getSource e)
          frame (.getTopLevelAncestor list-sn)]
      (handle-list-sn-event* list-sn frame))))

(defn handle-list-sn-mouse-clicked
  [e]
  (let [cc (.getClickCount e)
        list-sn (.getComponent e)
        frame (.getTopLevelAncestor list-sn)]
    (cond
     (= cc 1) (handle-list-sn-event* list-sn frame)
     (= cc 2) (when-not (.isSelectionEmpty list-sn)
                (let [sn (symbol (.getSelectedValue list-sn))
                      open-lib? (pos? (bit-and (.getModifiersEx e) InputEvent/META_DOWN_MASK))
                      alt-open? (pos? (bit-and (.getModifiersEx e) InputEvent/ALT_DOWN_MASK))]
                  (future
                    (let [oc (.getCursor frame)
                          alt-open-kwd (read-prop (get-props) 'slix.argkeyword.alt-open)]
                      (.setCursor frame Cursor/WAIT_CURSOR)
                      (try
                        (deref
                         (cond
                          open-lib? (open-slix-with-args {:file (get-slix-ns sn)} 'ced)
                          alt-open? (open-slix-with-args {alt-open-kwd true} sn)
                          :else (open-slix sn)))
                        (finally
                         (.setCursor frame oc)))))))
     :else nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-list-name-event*
  [list-name frame]
  (let [fprps (frame-props frame)]
    (when-not (get-prop fprps 'list.updating)
      ;; Prevent recursive valueChanged event loop
      (put-prop fprps 'list.updating true)
      ;; Start updating lists.
      (let [mpcs (get-main-panel-components frame)
            title-name-map (.getClientProperty list-name "title-name-map")
            sns-of-nms (reduce (fn [s o] (conj s o))
                               #{}
                               (map #(slix-sn (get-slix %)) (vals title-name-map)))
            name-titles (.getSelectedValues list-name)
            nms (map #(get title-name-map %) name-titles)
            sns-by-nms (get-registered-sns-by-names nms)
            ;;
            list-sn (:list-sn mpcs)
            lm (.getModel list-sn)
            sns-listed (map #(.getElementAt lm %) (range (.getSize lm)))
            ;;
            snlsls (seq (.getListSelectionListeners list-sn))
            nmlsls (seq (.getListSelectionListeners list-name))]
        ;; Update lists only when there are multiples sns.
        (when (< 1 (count sns-of-nms))
          (if-let [indecies (get-item-indices sns-listed sns-by-nms)]
            (do
              (.setListData list-name (into-array String name-titles))
              (.setSelectedIndices list-name (into-array Integer/TYPE (range (count name-titles))))
              (.setSelectedIndices list-sn (into-array Integer/TYPE indecies)))
            (.setListData list-name (into-array String []))))
        ;; Done
        (put-prop fprps 'list.updating false)))))

(defn handle-list-name-value-changed
  [e]
  (when-not (.getValueIsAdjusting e)
    (let [list-name (.getSource e)
          frame (.getTopLevelAncestor list-name)]
      (handle-list-name-event* list-name frame))))

(defn handle-list-name-mouse-clicked
  [e]
  (let [cc (.getClickCount e)
        list-name (.getComponent e)
        frame (.getTopLevelAncestor list-name)]
    (cond
     (= cc 1) (handle-list-name-event* list-name frame)
     (= cc 2) (let [title-name-map (.getClientProperty list-name "title-name-map")]
                (when-not (.isSelectionEmpty list-name)
                  (let [nm (get title-name-map (.getSelectedValue list-name))]
                    (.toFront (slix-frame (get-slix nm))))))
     :else nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize
  []
  (let [frame (slix-frame)
        mpcs (get-main-panel-components frame)]
    (put-prop (frame-props frame) 'list.updating false)
    ;;
    (.setText (:label-Sevenri mpcs) (get-sevenri-name-and-version))
    ;;
    (doto (:list-sn mpcs)
      (set-event-handler-set ListSelectionListener {'lsl ['valueChanged handle-list-sn-value-changed]}
                             MouseListener {'ml ['mouseClicked handle-list-sn-mouse-clicked]})
      (add-default-key-listener))
    (doto (:list-name mpcs)
      (set-event-handler-set ListSelectionListener {'lsl ['valueChanged handle-list-name-value-changed]}
                             MouseListener {'ml ['mouseClicked handle-list-name-mouse-clicked]})
      (add-default-key-listener))))

(defn update-sn-list
  []
  (let [slix *slix*]
    (invoke-later slix
     #(let [lsn (:list-sn (get-main-panel-components (slix-frame slix)))
            sns (sort (map str (get-all-slix-sn)))
            sis (get-selected-indices (seq (.getSelectedValues lsn)) sns)]
        (doto lsn
          (.setListData (into-array String sns))
          (.setSelectedIndices (into-array Integer/TYPE sis)))))))
