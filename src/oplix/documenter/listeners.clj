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

(ns oplix.documenter.listeners
  (:use [clojure.java browse]
        [openar log oplix]
        [oplix.documenter md2html mddb io ui])
  (:import (java.awt.event ActionListener)
           (javax.swing JOptionPane)
           (javax.swing.event DocumentListener PopupMenuListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *invalid-chars* "[<>:\"/\\|?*\\s]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contains??
  [items item]
  (if (some #(= (str %) item) items)
    true
    false))

(defn in-edit
  ([frame]
     (let [oplix (get-oplix frame)]
       (:documenter-in-edit (xref-with oplix))))
  ([frame stcvec]
     (let [oplix (get-oplix frame)
           stcvc (when (vector? stcvec) stcvec)]
       (add-to-xref oplix :documenter-in-edit stcvc)
       (invoke-later oplix
        #(update-title (when (= (count stcvc) 3) (first stcvc)))))))

(defn in-edit-by
  [frame stcvec]
  (when (vector? stcvec)
    (let [oplix (get-oplix frame)
          opvls (filter (fn [[o v]]
                          (and (not (identical? o oplix))
                               (<= (count stcvec) (count v))
                               (cond
                                (= (count stcvec) 1) (= (first stcvec) (last v))
                                (= (count stcvec) 2) (= stcvec (if (= (count v) 2)
                                                                 v
                                                                 (rest v)))
                                :else (= stcvec v))))
                        (xref-with :documenter-in-edit))]
      (seq opvls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Cetegory listeners

(defn get-category-list-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           clist (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor clist)
               ctrls (.getClientProperty clist *prop-controls*)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               cacts (:category-actions ctrls)
               tlist (:title-list ctrls)
               tacts (:title-actions ctrls)]
           (if (contains?? (get-categories) ctgry)
             (let [stcvec (in-edit frame)]
               (when (not= ctgry (last stcvec))
                 (in-edit frame [ctgry])
                 (enable-controls true [tlist tacts])
                 (set-list-items tlist (get-titles ctgry))
                 (let [slst (:section-list ctrls)
                       sect (:section-text ctrls)]
                   (.setSelectedItem slst -1)
                   (doto sect
                     (write-section true)
                     (.setText ""))
                   (doseq [c [slst sect (:publish ctrls) (:review ctrls)]]
                     (.setEnabled c false)))))
             (do
               (in-edit frame nil)
               (disable-controls (vals ctrls))
               (enable-controls true [clist cacts])
               (.setItem cedit ctgry)))))))))

(defn get-category-actions-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           cacts (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor cacts)
               ctrls (.getClientProperty cacts *prop-controls*)
               cactn (.getSelectedItem cacts)
               clist (:category-list ctrls)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               tlist (:title-list ctrls)
               tacts (:title-actions ctrls)]
           (when-not (empty? ctgry)
             (cond
              (= cactn "Add")
                (let [old-ctgry ctgry
                      ctgry (.replaceAll ctgry *invalid-chars* "_")]
                  (disable-controls (vals ctrls))
                  (when (add-category ctgry)
                    (update-mddb)
                    (enable-controls true [clist cacts])
                    (set-list-items clist (get-categories) ctgry)
                    (enable-controls true [tlist tacts])
                    (.requestFocusInWindow tlist)
                    (when (not= ctgry old-ctgry)
                      (.setSelectedItem clist ctgry)
                      (popup-invalid-char-message frame))))
              (= cactn "Delete")
                (let [ms (str "OK to delete " ctgry "?")
                      tl "Deleting Category"
                      ys (JOptionPane/showConfirmDialog frame ms tl JOptionPane/YES_NO_CANCEL_OPTION)]
                  (when (= ys JOptionPane/YES_OPTION)
                    (disable-controls (vals ctrls))
                    (when (remove-category ctgry)
                      (in-edit frame nil)
                      (update-mddb)
                      (set-list-items clist (get-categories))
                      (enable-controls true [clist cacts])
                      (.requestFocusInWindow clist))))
              (= cactn "Rename")
                (let [ms (str "Rename " ctgry " to:")
                      tl "Rename Category"
                      to (JOptionPane/showInputDialog frame ms tl
                                                      JOptionPane/PLAIN_MESSAGE)
                      to-ctgry (when (string? to) (.replaceAll to *invalid-chars* "_"))]
                  (when-not (empty? to-ctgry)
                    (if (get-category to-ctgry)
                      (JOptionPane/showMessageDialog frame (str to " exists already.")
                                                     tl JOptionPane/ERROR_MESSAGE)
                      (do
                        (disable-controls (vals ctrls))
                        (when (rename-category ctgry to-ctgry)
                          (in-edit frame [to-ctgry])
                          (update-mddb)
                          (enable-controls true [clist cacts])
                          (set-list-items clist (get-categories) to-ctgry)
                          (enable-controls true [tlist tacts])
                          (.requestFocusInWindow (:title-list ctrls))
                          (when (not= to to-ctgry)
                            (popup-invalid-char-message frame))))))))
             (.setSelectedIndex cacts 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Title listeners

(defn get-title-list-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           tlist (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor tlist)
               ctrls (.getClientProperty tlist *prop-controls*)
               tedit (.getEditor tlist)
               title (.getItem tedit)
               tacts (:title-actions ctrls)
               clist (:category-list ctrls)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               cacts (:category-actions ctrls)
               slist (:section-list ctrls)
               sacts (:section-actions ctrls)]
           (if (and (contains?? (get-titles ctgry) title)
                    (contains?? (get-categories) ctgry))
             (let [stcvec (in-edit frame)]
               (when (not= title (if (< (count stcvec) 3)
                                   (first stcvec)
                                   (second stcvec)))
                 (in-edit frame [title ctgry])
                 (enable-controls true [slist sacts])
                 (set-list-items slist (get-sections title ctgry))
                 (let [sect (:section-text ctrls)]
                   (doto sect
                     (write-section true)
                     (.setText ""))
                   (doseq [c [sect (:publish ctrls) (:review ctrls)]]
                     (.setEnabled c false)))))
             (do
               (in-edit frame nil)
               (disable-controls (vals ctrls))
               (enable-controls true [clist cacts])
               (if (contains?? (get-categories) ctgry)
                 (do
                   (.setSelectedItem clist ctgry)
                   (enable-controls true [tlist tacts])
                   (.setItem tedit title))
                 (do
                   (.setItem cedit ctgry)))))))))))

(defn get-title-actions-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           tacts (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor tacts)
               ctrls (.getClientProperty tacts *prop-controls*)
               tactn (.getSelectedItem tacts)
               tlist (:title-list ctrls)
               tedit (.getEditor tlist)
               title (.getItem tedit)
               clist (:category-list ctrls)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               cacts (:category-actions ctrls)
               slist (:section-list ctrls)
               sacts (:section-actions ctrls)]
           (when (and (not (empty? title))
                      (contains?? (get-categories) ctgry))
             (cond
              (= tactn "Add")
                (let [old-title title
                      title (.replaceAll title *invalid-chars* "_")]
                  (disable-controls (vals ctrls))
                  (when (add-title title ctgry)
                    (update-mddb)
                    (enable-controls true [clist cacts])
                    (set-list-items clist (get-categories) ctgry)
                    (enable-controls true [tlist tacts])
                    (set-list-items tlist (get-titles ctgry) title)
                    (enable-controls true [slist sacts])
                    (.requestFocusInWindow slist)
                    (when (not= title old-title)
                      (.setSelectedItem tlist title)
                      (popup-invalid-char-message frame))))
              (= tactn "Delete")
                (let [ms (str "OK to delete " title "?")
                      tl "Deleting Title"
                      ys (JOptionPane/showConfirmDialog frame ms tl JOptionPane/YES_NO_CANCEL_OPTION)]
                  (when (= ys JOptionPane/YES_OPTION)
                    (disable-controls (vals ctrls))
                    (when (remove-title title ctgry)
                      (in-edit frame [ctgry])
                      (update-mddb)
                      (enable-controls true [clist cacts])
                      (set-list-items clist (get-categories) ctgry)
                      (enable-controls true [tlist tacts])
                      (set-list-items tlist (get-titles ctgry))
                      (.requestFocusInWindow tlist))))
              (= tactn "Rename")
                (let [ms (str "Rename " title " to:")
                      tl "Rename Title"
                      to (JOptionPane/showInputDialog frame ms tl
                                                      JOptionPane/PLAIN_MESSAGE)
                      to-title (when (string? to) (.replaceAll to *invalid-chars* "_"))]
                  (when-not (empty? to-title)
                    (if (get-title to-title ctgry)
                      (JOptionPane/showMessageDialog frame (str to " exists already.")
                                                     tl JOptionPane/ERROR_MESSAGE)
                      (do
                        (disable-controls (vals ctrls))
                        (when (rename-title title to-title ctgry)
                          (in-edit frame [to-title ctgry])
                          (update-mddb)
                          (enable-controls true [clist cacts])
                          (set-list-items clist (get-categories) ctgry)
                          (enable-controls true [tlist tacts])
                          (set-list-items tlist (get-titles ctgry) to-title)
                          (enable-controls true [slist sacts])
                          (.requestFocusInWindow slist)
                          (when (not= to to-title)
                            (popup-invalid-char-message frame))))))))
             (.setSelectedIndex tacts 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sections listeners

(defn get-section-list-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           slist (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor slist)
               ctrls (.getClientProperty slist *prop-controls*)
               sedit (.getEditor slist)
               sectn (.getItem sedit)
               sacts (:section-actions ctrls)               
               tlist (:title-list ctrls)
               tedit (.getEditor tlist)
               title (.getItem tedit)
               tacts (:title-actions ctrls)
               clist (:category-list ctrls)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               cacts (:category-actions ctrls)
               stcvc [sectn title ctgry]]
           (if (and (contains?? (get-sections title ctgry) sectn)
                    (contains?? (get-titles ctgry) title)
                    (contains?? (get-categories) ctgry))
             (let [sectx (:section-text ctrls)
                   sctnf (get-src-section sectn title ctgry)
                   pbbtn (:publish ctrls)
                   rvbtn (:review ctrls)]
               (in-edit frame stcvc)
               (doseq [c [sectx pbbtn rvbtn]]
                 (.setEnabled c true))
               (doto sectx
                 (.setEditable true)
                 (read-section sctnf)
                 (.setCaretPosition 0)
                 (.requestFocusInWindow))
               (when-let [ovs (in-edit-by frame stcvc)]
                 ;; The same section is in edit by another documenter.
                 ;; Make the section read-only and disable Publish.
                 ;; Then bring the oplix which has editable text pane
                 ;; to the font.
                 (.setEditable sectx false)
                 (.setEnabled pbbtn false)
                 (when-first [ov (filter (fn [[o v]]
                                           (.isEditable (get-control o :section-text)))
                                         ovs)]
                   (invoke-later (first ov) #(.toFront (oplix-frame (first ov)))))))
             (do
               (in-edit frame nil)
               (disable-controls (vals ctrls))
               (enable-controls true [clist cacts])
               (if (contains? (get-categories) ctgry)
                 (if (contains? (get-titles ctgry) title)
                   (do
                     (.setSelectedItem clist ctgry)
                     (enable-controls true [tlist tacts])
                     (.setSelectedItem tlist title)
                     (enable-controls true [slist sacts])
                     (.setItem sedit sectn))
                   (do
                     (.setSelectedItem clist ctgry)
                     (enable-controls true [tlist tacts])
                     (.setItem tedit title)))
                 (.setItem cedit ctgry))))))))))

(defn get-section-actions-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           sacts (.getSource e)]
       (when (= accmd "comboBoxChanged")
         (let [frame (.getTopLevelAncestor sacts)
               ctrls (.getClientProperty sacts *prop-controls*)
               sactn (.getSelectedItem sacts)
               slist (:section-list ctrls)
               sedit (.getEditor slist)
               sectn (.getItem sedit)
               clist (:category-list ctrls)
               cedit (.getEditor clist)
               ctgry (.getItem cedit)
               cacts (:category-actions ctrls)
               tlist (:title-list ctrls)
               tedit (.getEditor tlist)
               title (.getItem tedit)
               tacts (:title-actions ctrls)
               sectx (:section-text ctrls)
               pbbtn (:publish ctrls)
               rvbtn (:review ctrls)]
           (when (and (not (empty? sectn))
                      (contains?? (get-titles ctgry) title)
                      (contains?? (get-categories) ctgry))
             (cond
              (= sactn "Add")
                (let [old-sectn sectn
                      sectn (.replaceAll sectn *invalid-chars* "_")]
                  (disable-controls (vals ctrls))
                  (when-let [sctnf (add-src-section sectn title ctgry)]
                    (in-edit frame [sectn title ctgry])
                    (add-out-section sectn title ctgry)
                    (add-res title ctgry)
                    (update-mddb)
                    (enable-controls true [clist cacts])
                    (set-list-items clist (get-categories) ctgry)
                    (enable-controls true [tlist tacts])
                    (set-list-items tlist (get-titles ctgry) title)
                    (enable-controls true [slist sacts])
                    (set-list-items slist (get-sections title ctgry) sectn)
                    (doseq [c [sectx pbbtn rvbtn]]
                      (.setEnabled c true))
                    (doto sectx
                      (.setEditable true)
                      (read-section sctnf)
                      (.requestFocusInWindow))
                    (when (not= sectn old-sectn)
                      (.setSelectedItem slist sectn)
                      (popup-invalid-char-message frame))))
              (= sactn "Delete")
                (let [ms (str "OK to delete " sectn "?")
                      tl "Deleting Section"
                      ys (JOptionPane/showConfirmDialog frame ms tl JOptionPane/YES_NO_CANCEL_OPTION)]
                  (when (= ys JOptionPane/YES_OPTION)
                    (disable-controls (vals ctrls))
                    (when (remove-src-section sectn title ctgry)
                      (remove-out-section sectn title ctgry)
                      (in-edit frame [title ctgry])
                      (update-mddb)
                      (enable-controls true [clist cacts])
                      (set-list-items clist (get-categories) ctgry)
                      (enable-controls true [tlist tacts])
                      (set-list-items tlist (get-titles ctgry) title)
                      (enable-controls true [slist sacts])
                      (set-list-items slist (get-sections title ctgry))
                      (.requestFocusInWindow slist))))
              (= sactn "Rename")
                (let [ms (str "Rename " sectn " to:")
                      tl "Rename Section"
                      to (JOptionPane/showInputDialog frame ms tl
                                                      JOptionPane/PLAIN_MESSAGE)
                      to-section (when (string? to) (.replaceAll to *invalid-chars* "_"))]
                  (when-not (empty? to-section)
                    (if (get-src-section to-section title ctgry)
                      (JOptionPane/showMessageDialog frame (str to " exists already.")
                                                     tl JOptionPane/ERROR_MESSAGE)
                      (do
                        (disable-controls (vals ctrls))
                        (when-let [sctnf (rename-src-section sectn to-section title ctgry)]
                          (rename-out-section sectn to-section title ctgry)
                          (in-edit frame [to-section title ctgry])
                          (update-mddb)
                          (enable-controls true [clist cacts])
                          (set-list-items clist (get-categories) ctgry)
                          (enable-controls true [tlist tacts])
                          (set-list-items tlist (get-titles ctgry) title)
                          (enable-controls true [slist sacts])
                          (set-list-items slist (get-sections title ctgry) to-section)
                          (doseq [c [sectx pbbtn rvbtn]]
                            (.setEnabled c true))
                          (doto sectx
                            (read-section sctnf)
                            (.requestFocusInWindow))
                          (when (not= to to-section)
                            (popup-invalid-char-message frame))))))))
             (.setSelectedIndex sacts 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Publish listeners

(defn get-publish-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           pbbtn (.getSource e)]
       (when (= accmd "Publish")
         (let [frame (.getTopLevelAncestor pbbtn)
               oplix (get-oplix frame)
               ctrls (.getClientProperty pbbtn *prop-controls*)
               clist (:category-list ctrls)
               ctgry (.getSelectedItem clist)
               tlist (:title-list ctrls)
               title (.getSelectedItem tlist)
               slist (:section-list ctrls)
               sectn (.getSelectedItem slist)
               secnf (get-src-section sectn title ctgry)
               sectx (:section-text ctrls)]
           (when (and secnf (= secnf (.getClientProperty sectx *prop-section-file*)))
             (future
               (write-section sectx)
               (when-let [html (markdown-to-html (.getText sectx))]
                 (when-let [osecn (get-out-section sectn title ctgry)]
                   (spit osecn html :encoding "UTF-8")
                   (invoke-later oplix
                     #(popup-publish-message frame))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Review listeners

(defn get-review-listener
  []
  (proxy [ActionListener] []
    (actionPerformed
     [e]
     (let [accmd (.getActionCommand e)
           rvbtn (.getSource e)]
       (when (= accmd "Review")
         (let [ctrls (.getClientProperty rvbtn *prop-controls*)
               clist (:category-list ctrls)
               ctgry (.getSelectedItem clist)
               tlist (:title-list ctrls)
               title (.getSelectedItem tlist)
               slist (:section-list ctrls)
               sectn (.getSelectedItem slist)
               secnf (get-src-section sectn title ctgry)
               sectx (:section-text ctrls)]
           (when (and secnf (= secnf (.getClientProperty sectx *prop-section-file*)))
             (when-let [osecn (get-out-section sectn title ctgry)]
               (browse-url (str "file://" osecn))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-list-updater
  [set-fn]
  (proxy [PopupMenuListener] []
    (popupMenuCanceled [e])
    (popupMenuWillBecomeInvisible [e])
    (popupMenuWillBecomeVisible [e]
      (set-fn))))

(defn get-action-menu-updater
  [action-control list-control get-fn]
  (fn []
    (doseq [i ["Add" "Delete" "Rename"]]
      (.removeItem action-control i))
    (let [edtr (.getEditor list-control)
          item (.getItem edtr)]
      (when (and (string? item) (not (empty? item)))
        (.addItem action-control "Add")
        (when (get-fn item)
          (doto action-control
            (.addItem "Delete")
            (.addItem "Rename")))))))

(defn get-action-updater
  [action-control list-control get-fn]
  (let [updater (get-action-menu-updater action-control list-control get-fn)]
    (proxy [PopupMenuListener] []
      (popupMenuCanceled [e])
      (popupMenuWillBecomeInvisible [e])
      (popupMenuWillBecomeVisible [e]
        (updater)))))

(defn get-list-document-listener
  [list-control action-control get-fn]
  (let [updater (get-action-menu-updater action-control list-control get-fn)]
    (proxy [DocumentListener] []
      (changedUpdate [e])
      (removeUpdate [e] (updater))
      (insertUpdate [e] (updater)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-listeners
  [frame]
  ;; Category
  (let [cls (get-control frame :category-list)
        cdc (.getDocument (.getEditorComponent (.getEditor cls)))
        cac (get-control frame :category-actions)
        ;;
        sfn (fn []
              (let [item (.getSelectedItem cls)]
                (set-list-items cls (get-categories) (or item -1))))
        gfn (fn [c]
              (and (get-category c)
                   (empty? (in-edit-by frame [c]))))]
    (.addActionListener cls (get-category-list-listener))
    (.addPopupMenuListener cls (get-list-updater sfn))
    (.addDocumentListener cdc (get-list-document-listener cls cac gfn))
    (.addActionListener cac (get-category-actions-listener))
    (.addPopupMenuListener cac (get-action-updater cac cls gfn)))
  ;; Title
  (let [cls (get-control frame :category-list)
        tls (get-control frame :title-list)
        tdc (.getDocument (.getEditorComponent (.getEditor tls)))
        tac (get-control frame :title-actions)
        ;;
        sfn (fn []
              (let [ctgry (.getSelectedItem cls)]
                (when (and (string? ctgry) (not (empty? ctgry)))
                  (let [item (.getSelectedItem tls)]
                    (set-list-items tls (get-titles ctgry) (or item -1))))))
        gfn (fn [t]
              (let [ctg (.getItem (.getEditor cls))]
                (and (get-title t ctg)
                     (empty? (in-edit-by frame [t ctg])))))]
    (.addActionListener tls (get-title-list-listener))
    (.addPopupMenuListener tls (get-list-updater sfn))
    (.addDocumentListener tdc (get-list-document-listener tls tac gfn))
    (.addActionListener tac (get-title-actions-listener))
    (.addPopupMenuListener tac (get-action-updater tac tls gfn)))
  ;; Section
  (let [cls (get-control frame :category-list)
        tls (get-control frame :title-list)
        sls (get-control frame :section-list)
        sdc (.getDocument (.getEditorComponent (.getEditor sls)))
        sac (get-control frame :section-actions)
        ;;
        sfn (fn []
              (let [ctgry (.getSelectedItem cls)
                    title (.getSelectedItem tls)]
                (when (and (string? ctgry) (not (empty? ctgry))
                           (string? title) (not (empty? title)))
                  (let [item (.getSelectedItem sls)]
                    (set-list-items sls (get-sections title ctgry) (or item -1))))))
        gfn (fn [s]
              (let [ctg (.getItem (.getEditor cls))
                    ttl (.getItem (.getEditor tls))]
                (and (get-src-section s ttl ctg)
                     (empty? (in-edit-by frame [s ttl ctg])))))]
    (.addActionListener sls (get-section-list-listener))
    (.addPopupMenuListener sls (get-list-updater sfn))
    (.addDocumentListener sdc (get-list-document-listener sls sac gfn))
    (.addActionListener sac (get-section-actions-listener))
    (.addPopupMenuListener sac (get-action-updater sac sls gfn)))
  ;; Publish and Review
  (let [pbn (get-control frame :publish)
        rbn (get-control frame :review)]
    (.addActionListener pbn (get-publish-listener))
    (.addActionListener rbn (get-review-listener))))
