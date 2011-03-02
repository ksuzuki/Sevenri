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

(ns slix.ced.listeners
  (:use [sevenri log]
        [sevenri.slix :only (invoke-later)]
        [slix.ced defs indent ui]
        [library.slix.ced.paren :only (find-matching-paren)])
  (:import (java.awt Event Point Rectangle)
           (java.awt.event ActionListener FocusListener KeyAdapter KeyEvent)
           (java.beans PropertyChangeListener)
           (javax.swing JSplitPane JViewport)
           (javax.swing.event CaretListener DocumentListener)
           (javax.swing.text DefaultCaret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *file-popup* (atom nil))
(def *fmp* (atom (future nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-line-number-and-column
  [ced]
  (let [cpos (.getCaretPosition ced)
        cdoc (.getDocument ced)
        delm (.getDefaultRootElement cdoc)
        lnum (inc (.getElementIndex delm cpos))
        pelm (.getParagraphElement cdoc cpos)
        soff (.getStartOffset pelm)
        coln (- cpos soff)
        lncl (.getClientProperty ced *prop-ced-line-number*)
        cncl (.getClientProperty ced *prop-ced-column-number*)]
    (.setText lncl (format "%d" lnum))
    (.setText cncl (format "%d" coln))))

(defn goto-line-centered
  [ced doc pos]
  (let [vprt (.getParent ced)
        relm (.getDefaultRootElement doc)
        ecnt (.getElementCount relm)
        pmax (.getEndOffset (.getElement relm (dec ecnt)))]
    (when (and (instance? JViewport vprt) (<= 0 pos) (<= pos pmax))
      (let [cidx (.getElementIndex relm pos)
            celm (.getElement relm cidx)
            crct (.modelToView ced pos)
            cdim (.getSize vprt)]
        (if (and (< (.height cdim) (* (.height crct) 3)))
          ;; The current view size is smaller to show three lines.
          ;; Show the line at the top of the view.
          (let [nrct (Rectangle. (.x crct) (.y crct) (.width cdim) (.height cdim))]
            (.setCaretPosition ced pos)
            (.scrollRectToVisible ced nrct))
          ;; Show the line in the center of the view.
          (let [enms (/ (.height cdim) (.height crct))
                teix (max 0 (- (inc cidx) (/ enms 2)))
                telm (.getElement relm teix)
                trct (.modelToView ced (.getStartOffset telm))
                nrct (Rectangle. (.x trct) (.y trct) (.width cdim) (.height cdim))]
            (.setCaretPosition ced pos)
            (.scrollRectToVisible ced nrct)))))))

(defn reset-find-start-position
  [ced]
  (let [doc (.getDocument ced)]
    (.setFindStartPos doc (.getCaretPosition ced))))

(defn find-next-string
  [ced doc next?]
  (let [[pos len] (.find doc next?)
        ccl (.getClientProperty ced *prop-ced-caret-listener*)]
    (.removeCaretListener ced ccl)
    (if (neg? pos)
      (.setCaretPosition ced len)
      (let [crt (.getCaret ced)]
        (doto ced
          (goto-line-centered doc pos)
          (.setCaretPosition (+ pos len))
          (.moveCaretPosition pos)
          (update-line-number-and-column))
        (.setSelectionVisible crt true)))
    (.addCaretListener ced ccl)))

(defn find-string
  [ced fstr-doc]
  (let [doc (.getDocument ced)
        str (.getText fstr-doc 0 (.getLength fstr-doc))]
    (.setFindString doc str)
    (find-next-string ced doc false)))

(defn find-back-to-start-pos
  [ced doc]
  (doto ced
    (goto-line-centered doc (.getFindStartPos doc))
    (.requestFocusInWindow)))

(defn find-matching-paren*
  "A wrapper of the find-matching-paren. When finding a matching opening paren
   failed, retry to find one from the beginning."
  ([ced pos]
     (let [[ppp fmpparam] (find-matching-paren ced pos)]
       (if (or ppp (first fmpparam) (zero? (second fmpparam)))
         [ppp fmpparam]
         (find-matching-paren ced pos {:beg-pos 0})))))

(defn highlight-matching-paren
  ([ced pos]
     (highlight-matching-paren ced pos false))
  ([ced pos clear?]
     (let [vrct (.getViewRect (.getParent ced))]
       ;; Sometimes vrct has weird values, such as negative hight.
       ;; Continue only when vrct has valid values.
       (if (or (neg? (.x vrct)) (neg? (.y vrct))
               (neg? (.width vrct)) (neg? (.height vrct)))
         (comment (log-warning "ced: highlight-matching-paren: invalid vrct:" vrct))
         ;; Finding maching paren can be expensive and time consuming.
         ;; Cancel the previous fmp future when it hasn't been done.
         (let [fmp (future (if clear? nil (find-matching-paren* ced pos)))]
           (when-not (future-done? @*fmp*)
             (future-cancel @*fmp*))
           (reset! *fmp* fmp)
           (future
             (while (not (or (future-done? fmp) (future-cancelled? fmp))))
             (when (future-done? fmp)
               (let [[new-ppp fmpparam] @fmp
                     [old-ppp old-clear?] (.getClientProperty ced *prop-ced-ppp-info*)]
                 #_(lg "ced:" (.getName ced) "old-ppp:" old-ppp "new-ppp:" new-ppp "clear?:" clear?)
                 (when (or (not= old-ppp new-ppp) (not= old-clear? clear?))
                   (.putClientProperty ced *prop-ced-ppp-info* [(if clear? old-ppp new-ppp) clear?])
                   (invoke-later (.getClientProperty ced *prop-ced-slix*)
                    #(.repaint ced (.getVisibleRect ced))))))))))))

(defn invoke-new-caret-position-actions
  [ced pos]
  (doto ced
    (highlight-matching-paren pos)
    (update-line-number-and-column))
  (predict-indent ced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ced-focus-listener
  [main-panel]
  (proxy [FocusListener] []
    (focusGained [e]
      (let [ced (.getComponent e)
            pos (.getCaretPosition ced)
            crt (.getCaret ced)
            ocd (.getClientProperty ced *prop-ced-other-ced*)
            ppp (.getClientProperty ocd *prop-ced-ppp-info*)]
        (.putClientProperty ocd *prop-ced-ppp-info* (when ppp [(first ppp) true]))
        (.putClientProperty main-panel *prop-mp-last-ced* ced)
        (.setAdjustVisibility crt true)
        (doto ced
          (highlight-matching-paren pos)
          (update-line-number-and-column))
        (predict-indent ced)))
    (focusLost [e]
      (let [ced (.getComponent e)
            pos (.getCaretPosition ced)
            crt (.getCaret ced)]
        (.setAdjustVisibility crt false)
        (highlight-matching-paren ced pos true)))))

(defn ced-caret-listener
  "Update the line and colum numbers."
  []
  (proxy [CaretListener] []
    (caretUpdate [e]
      (let [ced (.getSource e)
            pos (.getDot e)]
        (when (.isFocusOwner ced)
          (invoke-new-caret-position-actions ced pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn move-focus-key-listener
  "Moves the focus to the last ced, line-number, or find-string.
   Key      Focus on
   META+L:  line-number
   META+F:  find-string
   TAB/ESC: the last focused ced, if not on ced
   Note: make sure to disable FocusTraversalKey of the control to which
   this listener is assigned."
  [main-panel]
  (proxy [KeyAdapter] []
    (keyPressed [e]
      (when-not (.isConsumed e)
        (let [kc (.getKeyCode e)
              m? (pos? (bit-and (.getModifiers e) Event/META_MASK))
              cd (.getClientProperty main-panel *prop-mp-last-ced*)
              ln (.lineNumber main-panel)
              fs (.findString main-panel)
              tf (fn [e c]
                   (when-not (identical? c (.getComponent e))
                     (.consume e)
                     (.requestFocusInWindow c)))
              rf (fn [e c]
                   (.consume e)
                   (.requestFocusInWindow c))]
          (cond
           (= kc KeyEvent/VK_TAB) (tf e cd)
           (and (= kc KeyEvent/VK_ESCAPE) (.isFocusOwner ln)) (tf e cd)
           (and (= kc KeyEvent/VK_L) m?) (rf e ln)
           (and (= kc KeyEvent/VK_F) m?) (rf e fs)))))))

(defn divider-location-listener
  "Change the ced focus based on the divider location."
  [main-panel]
  (let [spl (.splitter main-panel)
        cd1 (.ced1 main-panel)
        cd2 (.ced2 main-panel)
        lnc (.lineNumber main-panel)
        cnc (.columnNumber main-panel)]
    (proxy [PropertyChangeListener] []
      (propertyChange [e]
        (let [min (.getMinimumDividerLocation spl)
              loc (.getNewValue e)
              max (.getMaximumDividerLocation spl)]
          (if (<= loc min)
            ;; The divider is at the top.
            (let [ppp (.getClientProperty cd2 *prop-ced-ppp-info*)]
              (.requestFocusInWindow cd1)
              (.putClientProperty cd2 *prop-ced-ppp-info* (when ppp [(first ppp) true])))
            (when (<= max loc)
              ;; The divider is at the bottom.
              (let [ppp (.getClientProperty cd1 *prop-ced-ppp-info*)]
                (.requestFocusInWindow cd2)
                (.putClientProperty cd1 *prop-ced-ppp-info* (when ppp [(first ppp) true]))))))))))

(defn line-number-action-listener
  [main-panel]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (let [str (.getActionCommand e)]
        (try
          (let [ln (Integer/parseInt str)
                ced (.getClientProperty main-panel *prop-mp-last-ced*)]
            (when (and (pos? ln) ced)
              (let [doc (.getDocument ced)
                    pos (.lineNumberToStartPosition doc ln)]
                (when-not (neg? pos)
                  (goto-line-centered ced doc pos)
                  (.requestFocusInWindow ced))))))))))

(defn line-number-focus-listener
  [main-panel]
  (proxy [FocusListener] []
    (focusGained [e]
      (when-not (.isTemporary e)
        (let [ln (.getComponent e)]
          (doto ln
            (.setText (.getText ln)) ;; hack to select all
            (.selectAll)))))
    (focusLost [e])))

(defn find-string-focus-listener
  [main-panel]
  (proxy [FocusListener] []
    (focusGained [e]
      (when-not (.isTemporary e)
        (reset-find-start-position (.getClientProperty main-panel *prop-mp-last-ced*))
        (let [fky (.getComponent e)
              doc (.getDocument fky)]
          (.setCaretPosition fky (max 0 (.getLength doc))))))
    (focusLost [e])))

(defn find-string-key-listener
  [main-panel]
  (proxy [KeyAdapter] []
    (keyPressed [e]
      (when-not (.isConsumed e)
        (let [kc (.getKeyCode e)
              ced (.getClientProperty main-panel *prop-mp-last-ced*)
              doc (.getDocument ced)]
          (cond
           (= kc KeyEvent/VK_ESCAPE)
             (find-back-to-start-pos ced doc)
           (or (= kc KeyEvent/VK_ENTER)
               (and (= kc KeyEvent/VK_G) (bit-and (.getModifiers e) Event/META_MASK)))
             (find-next-string ced doc true)))))))

(defn find-string-document-listener
  [main-panel]
  (proxy [DocumentListener] []
    (changedUpdate [e])
    (insertUpdate [e]
      (find-string (.getClientProperty main-panel *prop-mp-last-ced*) (.getDocument e)))
    (removeUpdate [e]
      (find-string (.getClientProperty main-panel *prop-mp-last-ced*) (.getDocument e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ced-listeners-and-properties
  [ced main-panel oced]
  (let [ccl (ced-caret-listener)]
    (doto ced
      (.addFocusListener (ced-focus-listener main-panel))
      (.addKeyListener (move-focus-key-listener main-panel))
      (.addCaretListener ccl)
      ;;
      (.putClientProperty *prop-ced-caret-listener* ccl)
      (.putClientProperty *prop-ced-column-number* (.columnNumber main-panel))
      (.putClientProperty *prop-ced-line-number* (.lineNumber main-panel))
      (.putClientProperty *prop-ced-find-string* (.findString main-panel))
      (.putClientProperty *prop-ced-other-ced* oced))))

(defn add-listeners
  [main-panel]
  (let [splt (.splitter main-panel)
        lnum (.lineNumber main-panel)
        fstr (.findString main-panel)
        mfkl (move-focus-key-listener main-panel)]
    (doto splt
      (.addPropertyChangeListener JSplitPane/DIVIDER_LOCATION_PROPERTY
                                  (divider-location-listener main-panel)))
    (doto lnum
      (.addKeyListener mfkl)
      (.addFocusListener (line-number-focus-listener main-panel))
      (.addActionListener (line-number-action-listener main-panel)))
    (doto fstr
      (.addKeyListener mfkl)
      (.addFocusListener (find-string-focus-listener main-panel))
      (.addKeyListener (find-string-key-listener main-panel)))
    (.addDocumentListener (.getDocument fstr) (find-string-document-listener main-panel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ced-key-listener
  [frame doc]
  (proxy [KeyAdapter] []
    (keyPressed [e]
      (when-not (.isConsumed e)
        (let [kc (.getKeyCode e)
              m? (pos? (bit-and (.getModifiers e) Event/META_MASK))
              mp (.getMousePosition frame false)]
          (when (and (= kc KeyEvent/VK_ALT) m? mp)
            (.consume e)
            (.show (save-ced-popup (create-ced-file-popup frame doc mp)))))))
    (keyReleased [e]
      (when-not (.isConsumed e)
        (let [kc (.getKeyCode e)]
          (when (or (= kc KeyEvent/VK_META) (= kc KeyEvent/VK_ALT))
            (.consume e)
            (clear-ced-popup)))))))

(defn add-ced-key-listener
  [comp frame doc]
  (.addKeyListener comp (get-ced-key-listener frame doc)))
