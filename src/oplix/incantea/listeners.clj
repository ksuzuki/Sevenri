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

(ns oplix.incantea.listeners
  (:use [openar config core log oplix ui utils]
        [oplix.incantea core defs])
  (:import (java.awt Cursor Rectangle)
           (java.awt.event ActionListener ContainerListener KeyAdapter KeyEvent)
           (java.io File)
           (javax.swing JInternalFrame JOptionPane)
           (javax.swing.event InternalFrameAdapter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-listeners
  [control getter remover]
  (doseq [l (seq (getter control))]
    (remover control l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn show-spell-in-title
  [list]
  (when-let [oplix (get-oplix (.getTopLevelAncestor list))]
    (let [spell (.getSelectedItem list)
          sfile (get-spell-file spell)]
      (add-to-xref oplix :incantea-file sfile)
      (set-oplix-title oplix (str (oplix-name oplix)
                                  (when-not (empty? spell)
                                    (str " - " spell)))))))

(defn add-spells-listener
  [list action items controls]
  (remove-listeners list #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   list
   (proxy [ActionListener] []
     (actionPerformed [e]
       (let [splname (.getSelectedItem list)
             enable? (not (empty? splname))]
         (doseq [c controls]
           (.setEnabled c enable?))
         (.removeItem action (:delete items))
         (when enable?
           (.addItem action (:delete items))
           ;; FIX ME; would this be useful?
           #_(binding [*ns* *ns*]
             (doto splname
               (in-spell-ns)
               (use-spell))))
         (show-spell-in-title list))))))

(defn update-spell-list
  ([list spell add?]
     (let [old-items (map #(.getItemAt list %) (range (.getItemCount list)))
           new-items (sort (if add?
                             (conj old-items spell)
                             (filter #(not (= spell %)) old-items)))]
       (update-spell-list list spell add? new-items)))
  ([list spell add? items]
     (let [act-lstrs (.getActionListeners list)]
       (doseq [al act-lstrs]
         (.removeActionListener list al))
       (.removeAllItems list)
       (doseq [i items]
         (.addItem list i))
       (doseq [al act-lstrs]
         (.addActionListener list al))
       ;;
       (if add?
         (.setSelectedItem list spell)
         (when (seq items)
           (.setSelectedIndex list 0))))))

(defn action-new-spell
  [actions list edit]
  (let [frm (.getTopLevelAncestor actions)
        msg "Type a new spell name:"
        spl (JOptionPane/showInputDialog frm msg (generate-new-spell-name))]
    (when-not (empty? spl)
      (let [f (get-spell-file spl)]
        (if (.exists f)
          (let [msg (str spl " exists already.")
                ttl "The Spell Exists"]
            (JOptionPane/showMessageDialog frm msg ttl JOptionPane/ERROR_MESSAGE))
          (with-create-on-get-dir
           (create-new-spell-file spl)
           (update-spell-list list spl true)
           (.doClick edit)))))))

(defn action-delete
  [actions list]
  (when-let [spl (.getSelectedItem list)]
    (let [frm (.getTopLevelAncestor actions)
          msg (str "OK to delete " spl "?")
          ttl "Deleting Spell"
          ync (JOptionPane/showConfirmDialog frm msg ttl
                                             JOptionPane/YES_NO_CANCEL_OPTION
                                             JOptionPane/QUESTION_MESSAGE)]
      (when (= ync JOptionPane/YES_OPTION)
        (let [oplix (get-oplix frm)
              sfile (get-spell-file spl)
              opkys (seq (filter (fn [[o k]] (not (identical? o oplix))) (xref-with sfile)))]
          (if (seq opkys)
            (let [msg (str spl " is opened by other oplixes.")
                  ttl "Cannot Delete Spell"]
              (JOptionPane/showMessageDialog frm msg ttl JOptionPane/ERROR_MESSAGE))
            (if (trash-file? sfile)
              (update-spell-list list spl false)
              (log-severe "incantea: failed to trash:" sfile))))))))

(defn add-spell-actions-listener
  [actions items list edit]
  (remove-listeners actions #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   actions
   (proxy [ActionListener] []
     (actionPerformed [e]
       (let [cmd (.getActionCommand e)]
         (when (= cmd "comboBoxChanged")
           (let [ait (.getSelectedItem actions)
                 spl (.getSelectedItem list)]
             (cond
              (= ait (:new-spell items)) (action-new-spell actions list edit)
              (= ait (:delete items)) (action-delete actions list))))
         (.setSelectedItem actions (:actions items)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-incant-listener
  [button spells oplix]
  (remove-listeners button #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   button
   (proxy [ActionListener] []
     (actionPerformed [e]
       (when-let [spell (.getSelectedItem spells)]
         (let [ls (load-spell spell)
               fr (.getTopLevelAncestor spells)
               cr (.getCursor fr)]
           (if (and (:loaded? ls) (:lib-ns-exists? ls) (:incant-defined? ls))
             (letfn [(pre-proc []
                       (.setEnabled button false)
                       (.setCursor fr Cursor/WAIT_CURSOR))
                     (post-proc []
                       (.setCursor fr cr)
                       (doto button
                         (.setEnabled true)
                         (.requestFocusInWindow)))]
               (future
                 (binding [*oplix* oplix]
                   (invoke-later pre-proc)
                   (try
                     ((ns-resolve (:lib-spell-ns ls) 'incant))
                     (catch Exception e
                       (log-exception e))
                     (finally
                      (invoke-later post-proc))))))
             (let [msg (cond
                        (not (:loaded? ls)) (str "Loading the spell failed.")
                        (not (:lib-ns-exists? ls)) (str "The spell library namespace " (:lib-spell-ns ls) " doesnot exist.")
                        (not (:incant-defined? ls)) (str "The incant function is not defined in the spell.")
                        :else "Unknown error")
                   ttl "Cannot Incant"]
               (JOptionPane/showMessageDialog fr msg ttl JOptionPane/ERROR_MESSAGE)))))))))

(defn add-edit-listener
  [button spells]
  (remove-listeners button #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   button
   (proxy [ActionListener] []
     (actionPerformed [e]
       (when-let [spell-file (when-let [spell (.getSelectedItem spells)]
                               (let [f (File. (get-spells-dir)
                                              (str (nssym2path (norm-spell-name spell)) ".clj"))]
                                 (when (.exists f)
                                   f)))]
         (open-oplix-with-args {:file spell-file} 'ced))
       (.requestFocusInWindow button)))))

(defn add-repl-listener
  [button oplix-name get-repl-name-fn]
  (remove-listeners button #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   button
   (proxy [ActionListener] []
     (actionPerformed [e]
       (when-let [rn (get-repl-name-fn oplix-name)]
         (let [ro (get-oplix rn)]
           (if ro
             (.toFront (oplix-frame ro))
             (open-oplix-with-args oplix-name 'repl rn))))
       (.requestFocusInWindow button)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn action-close-all
  ([desktop get-views-fn]
     (action-close-all desktop get-views-fn true))
  ([desktop get-views-fn confirm?]
     (let [ync (if confirm?
                 (let [frm (.getTopLevelAncestor desktop)
                       msg "OK to close all views?"
                       ttl "Closing All Views"]
                   (JOptionPane/showConfirmDialog frm msg ttl
                                                  JOptionPane/YES_NO_CANCEL_OPTION
                                                  JOptionPane/QUESTION_MESSAGE))
                 JOptionPane/YES_OPTION)]
       (when (= ync JOptionPane/YES_OPTION)
         (doseq [v (get-views-fn desktop)]
           (doto v
             (.setIcon false)
             (.dispose)))))))

(defn action-fit-in
  ([desktop get-views-fn]
     (action-fit-in desktop get-views-fn false))
  ([desktop get-views-fn all?]
     (let [bnds (.getBounds desktop nil)]
       (.setLocation bnds 0 0)
       (doseq [view (if all?
                      (get-views-fn desktop true)
                      (list (if-let [sf (.getSelectedFrame desktop)]
                              sf
                              (first (get-views-fn desktop)))))]
         (doto view
           (.setIcon false)
           (.setBounds bnds))))))

(defn action-iconify
  ([desktop get-views-fn]
     (action-iconify desktop get-views-fn false))
  ([desktop get-views-fn all?]
     (action-iconify desktop get-views-fn all? #(.setIcon % true)))
  ([desktop get-views-fn all? iconify-fn]
     (doseq [view (if all?
                    (get-views-fn desktop true)
                    (list (if-let [sf (.getSelectedFrame desktop)]
                              sf
                              (first (get-views-fn desktop)))))]
       (iconify-fn view))))

(defn action-deiconify
  ([desktop get-views-fn]
     (action-deiconify desktop get-views-fn false))
  ([desktop get-views-fn all?]
     (action-iconify desktop get-views-fn all? #(.setIcon % false))))

(defn action-tile
  [desktop get-views-fn axis]
  (let [vws (get-views-fn desktop true)
        vnm (count vws)
        dim (.getSize desktop)
        vww (/ (.getWidth  dim) (if (= axis :X) vnm 1))
        vwh (/ (.getHeight dim) (if (= axis :Y) vnm 1))
        bns (map #(Rectangle.
                   (+ 0 (* (if (= axis :X) vww 0) %))
                   (+ 0 (* (if (= axis :Y) vwh 0) %))
                   vww
                   vwh)
                 (range vnm))
        vbs (map #(vector %1 %2) vws bns)]
    (doseq [vb vbs]
      (let [[v b] vb]
        (when (.isIcon v)
          (.setIcon v false))
        (when (.isMaximum v)
          (.setMaximum v false))
        (.setBounds v b)))))

(defn dispatch-views-action
  [action desktop get-views-fn]
  (when-let [vafn (get {:close-all #(action-close-all desktop get-views-fn)
                        :close-all-yes #(action-close-all desktop get-views-fn false)
                        :fit-in #(action-fit-in desktop get-views-fn)
                        :fit-in-all #(action-fit-in desktop get-views-fn true)
                        :iconify #(action-iconify desktop get-views-fn)
                        :iconify-all #(action-iconify desktop get-views-fn true)
                        :deiconify #(action-deiconify desktop get-views-fn)
                        :deiconify-all #(action-deiconify desktop get-views-fn true)
                        :tile-horizontally #(action-tile desktop get-views-fn :X)
                        :tile-vertically #(action-tile desktop get-views-fn :Y)}
                       action)]
    (vafn)))

(defn add-views-action-listener
  [views items desktop get-views-fn]
  (remove-listeners views #(.getActionListeners %) #(.removeActionListener %1 %2))
  (.addActionListener
   views
   (proxy [ActionListener] []
     (actionPerformed [e]
       (let [cmd (.getActionCommand e)]
         (when (= cmd "comboBoxChanged")
           (let [ait (.getSelectedItem views)]
             (when-let [ak (get (apply hash-map (interleave (vals items) (keys items))) ait)]
               (dispatch-views-action ak desktop get-views-fn))))
         (.setSelectedItem views *views-title*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-key-listener
  [comp desktop get-views-fn control-map]
  (remove-listeners comp #(.getKeyListeners %) #(.removeKeyListener %1 %2))
  (.addKeyListener
   comp
   (proxy [KeyAdapter] []
     (keyPressed [e]
       (when-not (.isConsumed e)
         (let [kc (.getKeyCode e)
               md (.getModifiers e)]
           (when (pos? (bit-and md KeyEvent/VK_META))
             (letfn [(do-click [c e] (.doClick c) (.consume e))]
               (cond
                (= kc KeyEvent/VK_W) (let [view (or (.getSelectedFrame desktop)
                                                    (first (get-views-fn desktop)))]
                                       (if view
                                         (do
                                           (.dispose view)
                                           (.consume e))
                                         (default-key-pressed-listener e)))
                (= kc KeyEvent/VK_I) (do-click (:incant control-map) e)
                (= kc KeyEvent/VK_E) (do-click (:edit control-map) e)
                (= kc KeyEvent/VK_R) (do-click (:repl control-map) e)
                :else (default-key-pressed-listener e))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-view-listener
  [view desktop vactions register-view-fn unregister-view-fn]
  (.addInternalFrameListener
   view
   (proxy [InternalFrameAdapter] []
     (internalFrameOpened [e]
       (register-view-fn (.getInternalFrame e) desktop)
       (.setEnabled vactions true))
     (internalFrameClosed [e]
       (when (empty? (unregister-view-fn (.getInternalFrame e) desktop))
         (.setEnabled vactions false))
       (when (.isActive (.getTopLevelAncestor desktop))
         (.requestFocusInWindow desktop)))
     (internalFrameIconified [e]
       (when (.isActive (.getTopLevelAncestor desktop))
         (.requestFocusInWindow desktop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-desktop-listener
  [desktop]
  (remove-listeners desktop #(.getContainerListeners %) #(.removeContainerListener %1 %2))
  (.addContainerListener
   desktop
   (proxy [ContainerListener] []
     (componentAdded [e]
       (let [comp (.getChild e)]
         (when (instance? JInternalFrame comp)
           (.moveToFront desktop comp))))
     (componentRemoved [e]))))
