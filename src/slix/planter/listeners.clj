(ns slix.planter.listeners
  (:use [sevenri log slix ui]
        [slix.planter core controller io])
  (:import (java.awt.event ActionListener ItemEvent ItemListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-compile-listener
  [controls]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (let [accmd (.getActionCommand e)
            cpbtn (.getSource e)]
        (when (= accmd "Compile")
          (let [pn (get-project-name (:frame controls))]
            (lg "Compile" pn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-more-actions-listener
  [controls]
  (proxy [ItemListener] []
    (itemStateChanged [e]
      (let [src (.getSource e)
            itm (.getItem e)]
        (when (and (= (.getStateChange e) ItemEvent/SELECTED)
                   (not (zero? (.getSelectedIndex src))))
          (.setSelectedIndex src 0))))))

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
  [controls]
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
                (invoke-later slx #(set-title sel-proj slx))))))
        ;;
        (doseq [l itmlsnrs]
          (.addItemListener prjnames l))))))
