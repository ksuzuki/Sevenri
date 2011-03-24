(ns slix.planter.ui
  (:use [sevenri config log slix ui]
        [slix.planter defs io listeners])
  (:import (java.awt BorderLayout Color Cursor Dimension Font)
           (javax.swing BorderFactory Box BoxLayout JPanel)
           (javax.swing JButton JComboBox)
           (javax.swing JList JScrollPane JSplitPane JTextPane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ui-panel
  ([]
     (ui-panel *slix*))
  ([slix]
     (get-slix-prop slix :panel))
  ([slix panel]
     (put-slix-prop slix :panel panel)))

(defn ui-controls
  ([]
     (ui-controls *slix*))
  ([slix]
     (get-slix-prop slix :controls))
  ([slix controls]
     (put-slix-prop slix :controls controls)))

(defn enable-controls
  ([slix-or-frame]
     (enable-controls slix-or-frame true))
  ([slix-or-frame enable?]
     (let [slix (get-slix slix-or-frame)
           ctrls (get-slix-prop slix :controls)]
       (doseq [c (map #(get ctrls %) [:edit :compile :test :jar :more-actions :project-names])]
         (.setEnabled c enable?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn set-title
  ([sym]
     (set-title *slix* sym))
  ([slix sym]
     (set-slix-title (format "%s - %s" (slix-name slix) (str sym)))))

(defn set-cursor
  ([slix-or-frame]
     (let [slx (get-slix slix-or-frame)
           frm (slix-frame slx)
           rpn (.getRootPane frm)
           csr (.getClientProperty rpn *cursor*)]
       (set-cursor slx (or csr Cursor/DEFAULT_CURSOR))))
  ([slix-or-frame cursor]
     (let [slx (get-slix slix-or-frame)
           frm (slix-frame slx)
           rpn (.getRootPane frm)
           csr (.getCursor frm)]
       (.setCursor frm cursor)
       (.putClientProperty rpn *cursor* csr))))

(defn set-ui-wait
  ([slix-or-frame]
     (set-ui-wait slix-or-frame true nil))
  ([slix-or-frame wait? project-name]
     (enable-controls slix-or-frame (not wait?))
     (if wait?
       (set-cursor slix-or-frame Cursor/WAIT_CURSOR)
       (set-cursor slix-or-frame))
     (when-not wait?
       (declare init-ui)
       (let [slix (get-slix slix-or-frame)]
         (if (and (keyword? project-name) (= project-name :refresh))
           (invoke-later slix #(init-ui slix nil))
           (when project-name
             (invoke-later slix #(init-ui slix project-name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  ([]
     (add-ui *slix*))
  ([slix]
     (let [frame (slix-frame slix)
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
                  :edit edtbt
                  :compile cplbt
                  :test tstbt
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
             a (get-action-listener ctrls set-ui-wait)]
         (doseq [b [edtbt cplbt tstbt jarbt]]
           (.addActionListener b a)
           (.setMinimumSize b d)
           (.setMaximumSize b d)
           (.setPreferredSize b d)
           (.add btnbx b)))
       (.addItemListener macbx (get-more-actions-listener ctrls set-ui-wait))
       (.addItemListener prjns (get-project-name-listener ctrls set-title))
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
       (ui-panel slix panel)
       (ui-controls slix ctrls))))

(defn init-ui
  ([sym]
     (init-ui *slix* sym))
  ([slix sym]
     (let [nmcnfgmp (get-project-name-config-map)
           controls (ui-controls slix)
           prjnames (:project-names controls)
           itmlstrs (seq (.getItemListeners prjnames))]
       (doseq [l itmlstrs]
         (.removeItemListener prjnames l))
       ;;
       (.putClientProperty prjnames *name-config-map* nmcnfgmp)
       (.removeAllItems prjnames)
       (doseq [nm (sort (keys nmcnfgmp))]
         (.addItem prjnames (str nm)))
       ;; Do this or the setSelectedItem call below won't work.
       (.setSelectedIndex prjnames -1)
       ;;
       (doseq [l itmlstrs]
         (.addItemListener prjnames l))
       ;;
       (if sym
         (.setSelectedItem prjnames (str sym))
         (.setSelectedIndex prjnames 0))
       (.setDividerLocation (:splitter controls) 0.3)
       (set-title slix (.getSelectedItem prjnames)))))

(defn remove-ui
  ([]
     (remove-ui *slix*))
  ([slix]
     (let [frame (slix-frame slix)
           cpane (.getContentPane frame)]
       (.remove cpane 0))))

(defn restore-ui
  ([]
     (restore-ui *slix*))
  ([slix]
     (let [frame (slix-frame slix)
           cpane (.getContentPane frame)]
       (when-let [panel (ui-panel slix)]
         (.add cpane panel)))))
