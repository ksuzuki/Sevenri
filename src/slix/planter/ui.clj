(ns slix.planter.ui
  (:use [sevenri config log slix ui]
        [slix.planter controller defs io listeners])
  (:import (java.awt BorderLayout Color Dimension Font)
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

(defn add-ui
  []
  (let [frame (slix-frame)
        cpane (.getContentPane frame)
        ;;
        edtbt (JButton. "Edit")
        cplbt (JButton. "Compile")
        tstbt (JButton. "Test")
        jarbt (JButton. "Jar")
        btnbx (Box. BoxLayout/X_AXIS)
        macbx (JComboBox. (into-array *more-actions*))
        prjns (JComboBox.)
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
               :project-names prjns
               :config-text cfgtx
               :output-text outtx
               :splitter spltr}
        ;;
        panel (JPanel. (BorderLayout.))]
    ;;
    (let [ds (Dimension. (first *min-frame-size*) (second *min-frame-size*))]
      (doto frame
        (.setMinimumSize ds)
        (.setPreferredSize ds)))
    ;;
    (let [d (Dimension. (first *button-size*) (second *button-size*))
          a (get-action-listener ctrls)]
      (doseq [b [edtbt cplbt tstbt jarbt]]
        (.addActionListener b a)
        (.setMinimumSize b d)
        (.setMaximumSize b d)
        (.setPreferredSize b d)
        (.add btnbx b)))
    (.addItemListener macbx (get-more-actions-listener ctrls))
    (.addItemListener prjns (get-project-name-listener ctrls))
    (doto toppl
      (.add btnbx BorderLayout/WEST)
      (.add macbx BorderLayout/EAST)
      (.add prjns BorderLayout/SOUTH))
    ;;
    (let [font (get-font)]
      (doseq [text [cfgtx outtx]]
        (.setFont text font)
        (.setEditable text false)))
    (.setText outtx "\n")
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
