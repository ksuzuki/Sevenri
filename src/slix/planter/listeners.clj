(ns slix.planter.listeners
  (:use [sevenri log slix ui]
        [slix.planter core controller defs io])
  (:import (java.awt.event ActionListener ItemEvent ItemListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-lein-cmd
  [cmd]
  (let [cmd (get *lein-commands* (.toLowerCase cmd))]
    (or (get *command-aliases* cmd) cmd)))

(defn get-action-listener
  [controls set-ui-wait]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (let [accmd (.getActionCommand e)
            cpbtn (.getSource e)]
        (if-let [cmd (is-lein-cmd accmd)]
          (let [fr (:frame controls)
                ot (:output-text controls)
                pn (get-project-name fr)]
            (do-lein fr ot pn cmd set-ui-wait))
          (log-warning "planter: is-lein-cmd: unknown command:" accmd))))))

(defn get-more-actions-listener
  [controls set-ui-wait]
  (proxy [ItemListener] []
    (itemStateChanged [e]
      (let [src (.getSource e)
            itm (.getItem e)]
        (when (and (= (.getStateChange e) ItemEvent/SELECTED)
                   (not (zero? (.getSelectedIndex src))))
          (.setSelectedIndex src 0)
          (if-let [cmd (is-lein-cmd itm)]
            (let [fr (:frame controls)
                  ot (:output-text controls)
                  pn (get-project-name fr)]
              (do-lein fr ot pn cmd set-ui-wait))
            (when-first [cmd (filter #(= itm %) *more-actions*)]
              (do-command controls set-ui-wait cmd))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn show-config
  [controls project]
  (let [cfgtxt (:config-text controls)
        confgf (get (get-project-name-config-map) project)]
    (.setText cfgtxt "")
    (when (.exists confgf)
      (doto cfgtxt
        (.setText (slurp confgf :encoding "UTF-8"))
        (.setCaretPosition 0)))))

(defn get-project-name-listener
  [controls set-title]
  (proxy [ItemListener] []
    (itemStateChanged [e]
      (let [frm (:frame controls)
            slx (get-slix frm)
            cur-proj (get-project-name frm)
            sel-proj (symbol (.getItem e))
            prjnames (.getSource e)
            itmlsnrs (seq (.getItemListeners prjnames))]
        (doseq [l itmlsnrs]
          (.removeItemListener prjnames l))
        ;;
        (when (= (.getStateChange e) ItemEvent/SELECTED)
          #_(lg "cur-proj:" cur-proj "sel-proj:" sel-proj)
          (.setSelectedItem prjnames (str cur-proj))
          (if (= cur-proj sel-proj)
            (show-config controls cur-proj)
            ;;
            (if-let [slx (is-project-used sel-proj)]
              (.toFront (slix-frame slx))
              (when-not (is-lein-agent-busy? frm)
                (set-project-name slx sel-proj)
                (.setSelectedItem prjnames (str sel-proj))
                (show-config controls sel-proj)
                (invoke-later slx #(set-title slx sel-proj))))))
        ;;
        (doseq [l itmlsnrs]
          (.addItemListener prjnames l))))))
