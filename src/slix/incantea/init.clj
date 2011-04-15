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

(ns slix.incantea.init
  (:use [sevenri config event log slix ui]
        [slix.incantea core defs listeners ui])
  (:import (java.awt BorderLayout Dimension)
           (javax.swing Box BoxLayout)
           (javax.swing JButton JComboBox JLabel JPanel)
           (javax.swing JDesktopPane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-spell-name
  [spell-name]
  (with-create-sn-get-dir
    (let [sf (get-sid-slix-state-file)]
      (spit sf spell-name :encoding "UTF-8"))))

(defn load-spell-name
  []
  (let [sf (get-sid-slix-state-file)]
    (when (.exists sf)
      (slurp sf :encoding "UTF-8"))))

(defn save-state
  []
  (let [frame (slix-frame)
        panel (.getContentPane frame)
        tlbxy (.getComponent panel 0)
        tlbx1 (.getComponent tlbxy 0)
        splls (.getComponent tlbx1 1)
        splnm (.getSelectedItem splls)]
    (when-not (empty? splnm)
      (save-spell-name splnm))))

(defn load-state
  []
  {:spell-name (load-spell-name)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [frame (slix-frame)
        panel (JPanel. (BorderLayout.))
        tlbxy (Box. BoxLayout/Y_AXIS)
        ;;
        tlbx1 (Box. BoxLayout/X_AXIS)
        label (JLabel. " Spell:")
        splls (JComboBox.)
        sacts (JComboBox. (into-array (vector
                                       (:actions *action-items*)
                                       (:new-spell *action-items*))))
        ;;
        tlbx2 (Box. BoxLayout/X_AXIS)
        incnt (JButton. "Incant")
        edspl (JButton. "Edit")
        srepl (JButton. "Repl")
        hglue (Box/createHorizontalGlue)
        vacts (JComboBox. (into-array (apply vector *views-title*
                                             (vals *views-items*))))
        ;;
        dsktp (JDesktopPane.)]
    (doseq [c [label splls sacts]]
      (.add tlbx1 c))
    ;;
    (doseq [c [incnt edspl srepl hglue vacts]]
      (.add tlbx2 c))
    ;;
    (doseq [c [tlbx1 tlbx2]]
      (.add tlbxy c))
    ;;
    (doto panel
      (.add tlbxy BorderLayout/NORTH)
      (.add dsktp BorderLayout/CENTER))
    ;;
    (let [w *initial-frame-width*
          h *initial-frame-height*]
      (doto frame
        (.setContentPane panel)
        (.setSize w h)
        (.setMinimumSize (Dimension. w h))))))

(defn restore-ui
  []
  (let [frame (slix-frame)
        panel (.getContentPane frame)
        tlbxy (.getComponent panel 0)
        ;;
        tlbx1 (.getComponent tlbxy 0)
        label (.getComponent tlbx1 0)
        splls (.getComponent tlbx1 1)
        sacts (.getComponent tlbx1 2)
        ;;
        tlbx2 (.getComponent tlbxy 1)
        incnt (.getComponent tlbx2 0)
        edspl (.getComponent tlbx2 1)
        srepl (.getComponent tlbx2 2)
        hglue (.getComponent tlbx2 3)
        vacts (.getComponent tlbx2 4)
        ;;
        dsktp (.getComponent panel 1)]
    (doto dsktp
      (put-desktop)
      (add-desktop-listener))
    ;;
    (add-spells-listener splls sacts *action-items* [incnt edspl])
    (add-spell-actions-listener sacts *action-items* splls edspl)
    ;;
    (add-incant-listener incnt splls *slix*)
    (add-edit-listener edspl splls)
    (add-repl-listener srepl (slix-name) get-incantea-repl-name)
    (add-views-action-listener vacts *views-items* dsktp get-views)
    ;;
    (let [ier {:incant incnt :edit edspl :repl srepl :vactions vacts}]
      (-save-main-controls ier)
      (doseq [c [dsktp splls sacts incnt edspl srepl vacts]]
        (add-key-listener c dsktp get-views (-get-main-controls))))
    ;;
    (let [w *actions-width*
          h 160
          d (Dimension. w h)]
      (doto sacts
        (.setSize w h)
        (.setMinimumSize d)
        (.setMaximumSize d)))
    ;;
    (let [w *views-width*
          h 160
          d (Dimension. w h)]
      (doto vacts
        (.setSize w h)
        (.setMinimumSize d)
        (.setMaximumSize d)))))

(defn init-ui
  []
  (init-incanter)
  (restore-ui)
  (let [frame (slix-frame)
        panel (.getContentPane frame)
        tlbxy (.getComponent panel 0)
        ;;
        tlbx1 (.getComponent tlbxy 0)
        label (.getComponent tlbx1 0)
        splls (.getComponent tlbx1 1)
        sacts (.getComponent tlbx1 2)
        ;;
        tlbx2 (.getComponent tlbxy 1)
        incnt (.getComponent tlbx2 0)
        edspl (.getComponent tlbx2 1)
        srepl (.getComponent tlbx2 2)
        hglue (.getComponent tlbx2 3)
        vacts (.getComponent tlbx2 4)
        ;;
        splns (get-spell-names)
        stats (load-state)
        splnm (:spell-name stats)]
    (.setSelectedItem sacts (:actions *action-items*))
    (doseq [c [incnt edspl vacts]]
      (.setEnabled c false))
    (when (seq splns)
      (let [spell (if (and splnm (some #(= splnm %) splns))
                    splnm
                    (first splns))]
        (update-spell-list splls spell true splns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-incantea
  [event]
  (if (:sevenri.event/info-save-on-close (get-event-info event))
    (do
      (save-state)
      (do-views :close-all-yes))
    (create-event-response
     :sevenri.event/response-donot-save)))
