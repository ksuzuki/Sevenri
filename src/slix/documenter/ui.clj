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

(ns slix.documenter.ui
  (:use [sevenri log slix ui]
        [slix.documenter io])
  (:import (java.awt BorderLayout Color Dimension Font)
           (java.util.regex Pattern)
           (javax.swing BorderFactory Box BoxLayout PopupFactory)
           (javax.swing JButton JComboBox JFrame JLabel JOptionPane JPanel JScrollPane JTextPane)
           (javax.swing.border CompoundBorder)
           (javax.swing.text Segment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *prop-section-text* "section-text")
(def *prop-controls* "controls")

(def *control-names* [:category-list :category-actions
                      :title-list :title-actions
                      :section-list :section-actions
                      :section-text
                      :publish :review])

(def *section-find* "section-find")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [frm (slix-frame)
        cpn (.getContentPane frm)
        pnl (JPanel. (BorderLayout.))
        tpn (JTextPane.)
        spn (JScrollPane. tpn)
        sbd (BorderFactory/createLineBorder Color/white 2)
        mxd (Dimension. Integer/MAX_VALUE Integer/MAX_VALUE)]
    (.putClientProperty pnl *prop-section-text* tpn)
    (put-slix-prop *slix* :panel pnl)
    (doto tpn
      (add-default-key-listener)
      (.setMaximumSize mxd))
    (doto spn
      (.setViewportBorder sbd))
    (doto pnl
      (.add spn BorderLayout/CENTER))
    (doto cpn
      (.setBackground Color/gray)
      (.add pnl))
    (doto frm
      (.setMinimumSize (Dimension. 440 160))
      (.setMaximumSize mxd)
      (.setSize 600 400))))

(defn restore-ui
  []
  (let [frm (slix-frame)
        cpn (.getContentPane frm)
        pnl (.getComponent cpn 0)
        spn (.getComponent pnl 0)
        tpn (.getView (.getViewport spn))]
    (.putClientProperty pnl *prop-section-text* tpn)
    (put-slix-prop *slix* :panel pnl)))

(defn add-toolboxes
  []
  (let [pnl (get-slix-prop :panel)]
    (doseq [[box pos] (get-slix-prop :boxes)]
      (.add pnl box pos))
    (.validate pnl)))

(defn remove-toolboxes
  []
  (doseq [[box _] (get-slix-prop :boxes)]
    (.remove (get-slix-prop :panel) box)))

(defn add-panel-ui
  []
  (let [frm (slix-frame)
        cpn (.getContentPane frm)
        pnl (.getComponent cpn 0)
        fxd (Dimension. 64 16)
        mxd (Dimension. Integer/MAX_VALUE Integer/MAX_VALUE)
        acd (Dimension. 106 28)
        ;; bx1
        bx1 (Box. BoxLayout/Y_AXIS)
        acs (into-array ["Do..."])
        ;; Category controls
        xb1 (Box. BoxLayout/X_AXIS)
        cat (JLabel. "Category:" JLabel/RIGHT)
        cls (JComboBox.)
        cac (JComboBox. acs)
        ;; Title controls
        xb2 (Box. BoxLayout/X_AXIS)
        ttl (JLabel. "Title:" JLabel/RIGHT)
        tls (JComboBox.)
        tac (JComboBox. acs)
        ;; Section controls
        xb3 (Box. BoxLayout/X_AXIS)
        sct (JLabel. "Section:" JLabel/RIGHT)
        sls (JComboBox.)
        sac (JComboBox. acs)
        ;; bx2
        bx2 (Box. BoxLayout/X_AXIS)
        ;; Publish button
        pbn (JButton. "Publish")
        rbn (JButton. "Review")
        blk (Box/createHorizontalGlue)]
    ;; bx1
    (doseq [c [cat ttl sct]]
      (.setSize c fxd)
      (.setMinimumSize c fxd)
      (.setMaximumSize c fxd)
      (.setPreferredSize c fxd))
    (doseq [c [cls tls sls]]
      (add-default-key-listener (.getEditorComponent (.getEditor c)))
      (.setMaximumSize c mxd)
      (.setEditable c true))
    (doseq [c [cac tac sac]]
      (.setSize c fxd)
      (.setMinimumSize c acd)
      (.setMaximumSize c acd)
      (.setPreferredSize c acd)
      (add-default-key-listener c))
    (doseq [c [cat cls cac]]
      (.add xb1 c))
    (doseq [c [ttl tls tac]]
      (.add xb2 c))
    (doseq [c [sct sls sac]]
      (.add xb3 c))
    (doseq [c [xb1 xb2 xb3]]
      (.add bx1 c))
    ;; bx2
    (doseq [c [pbn rbn]]
      (.add bx2 c))
    ;;
    (let [stx (.getClientProperty pnl *prop-section-text*)
          ctrls (apply hash-map (interleave *control-names*
                                            [cls cac tls tac sls sac stx pbn rbn]))]
      (doseq [c [pnl bx1 bx2 cls cac tls tac sls sac pbn rbn]]
        (.putClientProperty c *prop-controls* ctrls))
      (declare disable-controls)
      (disable-controls (vals ctrls)))
    ;;
    (put-slix-prop *slix* :panel pnl :boxes [[bx1 BorderLayout/NORTH]
                                               [bx2 BorderLayout/SOUTH]])
    (add-toolboxes)))

(defn update-title
  [section]
  (let [title (str (slix-name) (when section (str " - " section)))]
    (set-slix-title title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-control
  ([storage ctrl-kwd]
  (if (is-slix? storage)
    (get-control storage ctrl-kwd (get-slix-prop storage :panel))
    (if (instance? JFrame storage)
      (get-control storage ctrl-kwd (.getComponent (.getContentPane storage) 0))
      (if (instance? JPanel storage)
        (get-control storage ctrl-kwd storage)
        (throw (IllegalArgumentException. "documenter: invalid storage for get-control"))))))
  ([storage ctrl-kwd panel]
     (let [ctrl (when-let [ctrls (.getClientProperty panel *prop-controls*)]
                  (if (= ctrl-kwd :controls)
                    ctrls
                    (ctrl-kwd ctrls)))]
       (or ctrl
           (throw (IllegalArgumentException. (str "documenter: get-control failed: " ctrl-kwd)))))))

(defn get-controls
  [storage]
  (get-control storage :controls))

(defn enable-controls
  [enable? controls]
  (doseq [c controls]
    (.setEnabled c enable?)))

(defn disable-controls
  [controls]
  (doseq [c controls]
    (let [acs (when (instance? JComboBox c)
                (.getActionListeners c))]
      (doseq [ac (seq acs)]
        (.removeActionListener c ac))
      ;;
      (when (and (instance? JComboBox c) (.isEditable c))
        (.setSelectedItem c nil))
      (when (instance? JTextPane c)
        (write-section c true)
        (.setText c ""))
      (.setEnabled c false)
      ;;
      (doseq [ac (seq acs)]
        (.addActionListener c ac)))))

(defn set-list-items
  ([control items]
     (set-list-items control items -1))
  ([control items index]
     (let [acs (.getActionListeners control)]
       (doseq [ac (seq acs)]
         (.removeActionListener control ac))
       (.removeAllItems control)
       (let [sis (sort (map str items))]
         (doseq [i sis]
           (.addItem control i))
         (.setSelectedIndex control (if (number? index)
                                      index
                                      (get (apply hash-map (interleave sis (range))) index))))
       (doseq [ac (seq acs)]
         (.addActionListener control ac)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-current-list-item
  [list-control]
  (.getItem (.getEditor list-control)))

(defn get-current-category
  [frame]
  (let [category (get-current-list-item (get-control frame :category-list))]
    (when (get-category category)
      category)))

(defn get-current-title-category
  [frame]
  (when-let [category (get-current-category frame)]
    (let [title (get-current-list-item (get-control frame :title-list))]
      (when (get-title title category)
        [title category]))))

(defn get-current-section-title-category
  [frame]
  (when-let [title-category (get-current-title-category frame)]
    (let [section (get-current-list-item (get-control frame :section-list))
          [title category] title-category]
      (when (get-src-section section title category)
        [section title category]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn popup-message
  [frame msg msec]
  (let [loc (.getLocationOnScreen frame)
        lcx (+ (.x loc) 2)
        lcy (+ (.y loc) 2)
        lbl (JLabel.)
        fgc Color/black
        bgc (Color. 255 255 204)
        pup (.getPopup (PopupFactory/getSharedInstance) frame lbl lcx lcy)]
    (doto lbl
      (.setOpaque true)
      (.setFont (Font. "Helvetica" Font/PLAIN 14))
      (.setText msg)
      (.setBorder (BorderFactory/createLineBorder bgc 2))
      (.setForeground fgc)
      (.setBackground bgc))
    (future (.show pup)
            (Thread/sleep msec)
            (.hide pup))))

(defn popup-save-message
  [frame]
  (popup-message frame " Saved " 500))

(defn popup-publish-message
  [frame]
  (popup-message frame " Save & published " 500))

(defn popup-invalid-char-message
  [frame]
  (popup-message frame "Replaced invalid chars with '_'" 3000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-next-in-section
  [section-text]
  (let [fstr (.getClientProperty section-text *section-find*)]
    (when-not (empty? fstr)
      (let [ptrn (Pattern/compile fstr (bit-or Pattern/CASE_INSENSITIVE Pattern/LITERAL))
            tseg (Segment.)
            sdoc (.getStyledDocument section-text)]
        (loop [tbeg (.getCaretPosition section-text)
               tlen (- (.getLength sdoc) tbeg)]
          (when (<= (+ tbeg tlen) (.getLength sdoc))
            (.getText sdoc tbeg tlen tseg)
            (let [mtchr (.matcher ptrn (.subSequence tseg 0 (.length tseg)))]
              (when (.find mtchr)
                (let [mrslt (.toMatchResult mtchr)
                      mtbeg (.start mrslt)
                      mtend (.end mrslt)
                      slbeg (.getSelectionStart section-text)
                      slend (.getSelectionEnd section-text)]
                  (if (and (not= mtbeg slbeg) (not= mtend slend))
                    (doto section-text
                      (.setCaretPosition (+ tbeg mtbeg))
                      (.moveCaretPosition (+ tbeg mtend)))
                    (recur (inc mtend) (- (.getLength sdoc) (inc mtend)))))))))))))

(defn find-in-section
  [section-text]
  (let [dw-frame (.getTopLevelAncestor section-text)
        old-fstr (str (.getClientProperty section-text *section-find*))
        new-fstr (JOptionPane/showInputDialog dw-frame old-fstr "Find" JOptionPane/QUESTION_MESSAGE
                                              nil nil old-fstr)]
    (when-not (empty? new-fstr)
      (.putClientProperty section-text *section-find* new-fstr)
      (find-next-in-section section-text))))
