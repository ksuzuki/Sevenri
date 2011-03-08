(ns slix.planter.ui
  (:use [sevenri config log slix ui]
        [slix.planter controller defs io])
  (:import (java.awt BorderLayout Color Dimension Font)
           (java.awt.event ItemEvent ItemListener)
           (javax.swing BorderFactory Box BoxLayout JPanel)
           (javax.swing JButton JComboBox)
           (javax.swing JList JScrollPane JSplitPane JTextPane)))

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

(defn set-title
  ([sym]
     (set-title sym *slix*))
  ([sym slix]
     (set-slix-title (format "%s - %s" (slix-name slix) (str sym)))))

(defn get-font
  "This should be merged into sevenri.ui."
  []
  (let [lge (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
        pref (filter #(= (ffirst *preferred-fonts*) %)
                     (map str (seq (.getAvailableFontFamilyNames lge))))
        [name style size] (if (seq pref)
                             (first *preferred-fonts*)
                             (second *preferred-fonts*))]
    (Font. name style size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-more-actions-item-listener
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

(defn get-project-name-item-listener
  [controls]
  (proxy [ItemListener] []
    (itemStateChanged [e]
      (let [frm (:frame controls)
            slx (get-slix frm)
            cur-proj (get-project-name frm)
            sel-proj (symbol (.getItem e))
            name-lst (.getSource e)
            itmlsnrs (seq (.getItemListeners name-lst))]
        (doseq [l itmlsnrs]
          (.removeItemListener name-lst l))
        ;;
        (when (= (.getStateChange e) ItemEvent/SELECTED)
          (.setSelectedItem name-lst (str cur-proj))
          (if (= cur-proj sel-proj)
            (show-config controls cur-proj)
            ;;
            (if-let [slx (is-project-used sel-proj)]
              (.toFront (slix-frame slx))
              (when-not (is-project-busy? frm)
                (set-project-name slx sel-proj)
                (.setSelectedItem name-lst (str sel-proj))
                (show-config controls sel-proj)
                (invoke-later slx #(set-title sel-proj slx))))))
        ;;
        (doseq [l itmlsnrs]
          (.addItemListener name-lst l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [frame (slix-frame)
        cpane (.getContentPane frame)
        ;;
        edtbt (JButton. "Edit")
        cplbt (JButton. "Compile")
        jarbt (JButton. "Jar")
        btnbx (Box. BoxLayout/X_AXIS)
        macbx (JComboBox. (into-array *more-actions*))
        prjnl (JComboBox.)
        toppl (JPanel. (BorderLayout.))
        ;;
        cfgtx (JTextPane.)
        cfgpn (JPanel. (BorderLayout.))
        cfgsp (JScrollPane. cfgpn)
        outtx (JTextPane.)
        outpn (JPanel. (BorderLayout.))
        outsp (JScrollPane. outpn)
        spltr (JSplitPane. JSplitPane/VERTICAL_SPLIT true cfgsp outsp)
        ;;
        ctrls {:frame frame
               :compile cplbt
               :jar jarbt
               :more-actions macbx
               :name-list prjnl
               :config-text cfgtx
               :output-text outtx
               :splitter spltr}
        ;;
        panel (JPanel. (BorderLayout.))]
    ;;
    (let [ds (Dimension. 510 320)]
      (doto frame
        (.setMinimumSize ds)
        (.setPreferredSize ds)))
    ;;
    (let [d (Dimension. (first *button-size*) (second *button-size*))]
      (doseq [b [edtbt cplbt jarbt]]
        (.setMinimumSize b d)
        (.setMaximumSize b d)
        (.setPreferredSize b d)
        (.add btnbx b)))
    (.addItemListener macbx (get-more-actions-item-listener ctrls))
    (.addItemListener prjnl (get-project-name-item-listener ctrls))
    (doto toppl
      (.add btnbx BorderLayout/WEST)
      (.add macbx BorderLayout/EAST)
      (.add prjnl BorderLayout/SOUTH))
    ;;
    (let [font (get-font)]
      (doseq [text [cfgtx outtx]]
        (.setFont text font)
        (.setEditable text false)))
    (.add cfgpn cfgtx)
    (.add outpn outtx)
    ;;
    (doto panel
      (.setBorder (BorderFactory/createLineBorder Color/lightGray 2))
      (.add toppl BorderLayout/NORTH)
      (.add spltr BorderLayout/CENTER))
    ;;
    (.add cpane panel)
    (ui-panel panel)
    (ui-controls ctrls)))

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
