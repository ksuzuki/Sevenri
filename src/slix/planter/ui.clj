(ns slix.planter.ui
  (:use [sevenri config log slix ui]
        [slix.planter defs io])
  (:import (java.awt BorderLayout Color Dimension)
           (java.awt.event ItemEvent ItemListener)
           (javax.swing BorderFactory DefaultListModel JPanel)
           (javax.swing JButton JComboBox)
           (javax.swing JList JScrollPane JSplitPane JTextPane)
           (javax.swing.event ListSelectionListener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ui-panel
  ([]
     (get-slix-prop :panel))
  ([panel]
     (put-slix-prop :panel panel)))

(defn ui-controls
  ([]
     (get-slix-prop :controls))
  ([controls]
     (put-slix-prop :controls controls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-more-actions-item-listener
  [controls]
  (proxy [ItemListener] []
    (itemStateChanged [e]
      (let [src (.getSource e)
            itm (.getItem e)]
        (when (and (= (.getStateChange e) ItemEvent/SELECTED)
                   (not (zero? (.getSelectedIndex src))))
          (lg "gmail:" itm)
          (.setSelectedIndex src 0))))))

(defn get-project-name-selection-listener
  [controls]
  (proxy [ListSelectionListener] []
    (valueChanged [e]
      (let [idcs (seq (.getSelectedIndices (.getSource e)))
            txbx (:text-box controls)]
        (.setText txbx "")
        (when (= (count idcs) 1)
          (let [pname (.getSelectedValue (.getSource e))
                nmlst (:name-list controls)
                ncmap (.getClientProperty nmlst *name-config-map*)]
            (when ncmap
              (let [f (get ncmap (symbol pname))]
                (when (.exists f)
                  (doto txbx
                    (.setText (slurp f :encoding "UTF-8"))
                    (.setCaretPosition 0)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [frame (slix-frame)
        cpane (.getContentPane frame)
        ;;
        opbtn (JButton. "Open")
        mabtn (JComboBox.)
        btnpl (JPanel. (BorderLayout.))
        ;;
        prjnl (JList. (DefaultListModel.))
        prjtx (JTextPane.)
        scrpn (JScrollPane. prjtx)
        spltr (JSplitPane. JSplitPane/VERTICAL_SPLIT true prjnl scrpn)
        ;;
        panel (JPanel. (BorderLayout.))
        ctrls {:name-list prjnl :splitter spltr :text-box prjtx}]
    ;;
    (let [df (get-default :frame)
          ds (Dimension. (:width df) (:height df))]
      (.setMinimumSize frame ds))
    ;;
    (doseq [i ["More Actions..." "Delete..." "New..."]]
      (.addItem mabtn i))
    (.addItemListener mabtn (get-more-actions-item-listener ctrls))
    (doto btnpl
      (.add opbtn BorderLayout/WEST)
      (.add mabtn BorderLayout/EAST))
    ;;
    (.addListSelectionListener prjnl (get-project-name-selection-listener ctrls))
    (.setEditable prjtx false)
    ;;
    (doto panel
      (.setBorder (BorderFactory/createLineBorder Color/lightGray 2))
      (.add btnpl BorderLayout/NORTH)
      (.add spltr BorderLayout/CENTER))
    ;;
    (.add cpane panel)
    (ui-panel panel)
    (ui-controls ctrls)))

(defn init-ui
  []
  (let [controls (ui-controls)
        name-lst (:name-list controls)
        list-mdl (.getModel name-lst)
        nmcnfgmp (get-project-name-config-map)]
    (.putClientProperty name-lst *name-config-map* nmcnfgmp)
    (.clear list-mdl)
    (doseq [nm (sort (keys nmcnfgmp))]
      (.addElement list-mdl (str nm)))
    (.setDividerLocation (:splitter controls) 0.3)
    (.setSelectedIndex name-lst 0)))

(defn remove-ui
  []
  (let [frame (slix-frame)
        cpane (.getContentPane frame)]
    (.remove cpane 0)))

(defn restore-ui
  []
  (let [frame (slix-frame)
        cpane (.getContentPane frame)]
    (when-let [panel (ui-panel)]
      (.add cpane panel))))
