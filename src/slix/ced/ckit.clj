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

(ns slix.ced.ckit
  (:gen-class
   :extends javax.swing.text.DefaultEditorKit
   :exposes-methods {getActions superGetActions}
   :main false)
  (:use [sevenri config core log slix]
        [slix.ced defs listeners indent]
        [slix.ced.find :only (find-context-namespace)]
        [slix.ced.ui :only (update-title popup-warning)]
        [library.slix.ced doclib])
  (:import (java.awt FileDialog)
           (java.awt.event FocusEvent)
           (java.io File FilenameFilter)
           (javax.swing AbstractAction Action JEditorPane UIManager)
           (javax.swing.text DefaultEditorKit
                             DefaultEditorKit$InsertTabAction
                             DefaultEditorKit$InsertBreakAction
                             NavigationFilter PlainView Position
                             TextAction ViewFactory)
           (javax.swing.undo UndoManager)
           ;;
           (slix.ced cdoc cview)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn disable-caret-update
  [ced]
  (let [navf (.getNavigationFilter ced)
        tnvf (proxy [NavigationFilter] []
               (getNextVisualPositionFrom [text pos bias direction bias-ret]
                 pos)
               (moveDot [fb dot bias])
               (setDot [fb dot bias]))]
    (.setNavigationFilter ced tnvf)
    navf))

(defn enable-caret-update
  [ced nav-filter]
  (.setNavigationFilter ced nav-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ced-action
  [event & body]
  `(when ~event
     (when-let [~'ced (.getSource ~event)]
       (when (instance? ~'JEditorPane ~'ced)
         ~@body))))

(defmacro atomic-undoable-action
  [ced & body]
  `(let [doc# (.getDocument ~ced)
         udm# (.getUndoMan doc#)
         tud# (~'UndoManager.)]
     (.removeUndoableEditListener doc# udm#)
     (.addUndoableEditListener doc# tud#)
     ~@body
     (.removeUndoableEditListener doc# tud#)
     (.end tud#)
     (.addEdit udm# tud#)
     (.addUndoableEditListener doc# udm#)))

(defn do-indent-region
  "Indent the lines when there are multiple ones selected and the ones which
   are selected whole."
  [ced]
  (let [beg (.getSelectionStart ced)
        end (.getSelectionEnd ced)
        bfc (first-char beg)
        efc (first-char end)
        bli (let [li (line-index beg)]
              (if bfc
                (if (<= beg (first bfc)) li (inc li))
                (inc li)))
        eli (let [li (line-index end)]
              (if efc
                (if (<= (first efc) end) li (dec li))
                (dec li)))]
    (when (< bli eli)
      (let [doc (get-doc-context :doc)
            elm (get-doc-context :doc-elm)
            pos (.createPosition doc (get-doc-context :pos))
            ;;
            ctls [(.getClientProperty ced *prop-ced-line-number*)
                  (.getClientProperty ced *prop-ced-find-keyword*)]
            ceds [ced (.getClientProperty ced *prop-ced-other-ced*)]
            crls (.getCaretListeners ced)
            frls (.getFocusListeners ced)
            oplt (.getClientProperty ced *prop-ced-slix*)]
        (future
          (invoke-and-wait oplt
            #(do
               (doseq [ct ctls] (.setEnabled ct false))
               (doseq [cd ceds] (.setEditable cd false))
               (doseq [cl crls] (.removeCaretListener ced cl))
               (doseq [fl frls] (.removeFocusListener ced fl))))
          ;;
          (let [ubo-contxt (atom nil)]
            (doseq [li (range bli (inc eli))]
              (invoke-and-wait oplt
                #(setup-doc-context ced nil
                   (let [pos (min (dec (get-doc-context :max-pos))
                                  (.getStartOffset (.getElement elm li)))]
                     (.setCaretPosition ced pos)
                     (reset! ubo-contxt (indent-region ced pos @ubo-contxt)))))))
          ;;
          (invoke-and-wait oplt
            #(do
               (doseq [fl frls] (.addFocusListener ced fl))
               (doseq [cl crls] (.addCaretListener ced cl))
               (doseq [cd ceds] (.setEditable cd true))
               (doseq [ct ctls] (.setEnabled ct true))
               (.setCaretPosition ced (.getOffset pos))))
          ;;
          (invoke-later oplt
            #(invoke-new-caret-position-actions ced (.getCaretPosition ced))))))))

(defn do-indent
  "When the current pos is after the first char pos, indent prediction is
   invalid and a new indent column has to be calculated again."
  [ced]
  (let [cls (.getCaretListeners ced)]
    (doseq [cl cls] (.removeCaretListener ced cl))
    ;;
    (let [pos (get-doc-context :pos)
          fch (first-char pos)
          fcp (when (and fch (< (first fch) pos))
                (first fch))
          col (if fcp
                (do
                  (.setCaretPosition ced (start-of-line pos))
                  (get-indent-column ced false))
                (get-indent-column ced))]
      (when-let [npos (indent ced col)]
        (.setCaretPosition ced npos)))
    ;;
    (doseq [cl cls] (.addCaretListener ced cl))
    (invoke-new-caret-position-actions ced (.getCaretPosition ced))))

(defn insert-tab-action
  []
  (proxy [DefaultEditorKit$InsertTabAction] []
    (actionPerformed [event]
      (ced-action event
        (atomic-undoable-action ced
          (setup-doc-context ced nil
            (if (.getSelectedText ced)
              (do-indent-region ced)
              (do-indent ced))))))))

(defn insert-break-action
  []
  (proxy [DefaultEditorKit$InsertBreakAction] []
    (actionPerformed [event]
      (ced-action event
        (atomic-undoable-action ced
          (setup-doc-context ced nil
            (let [doc (get-doc-context :doc)
                  pos (get-doc-context :pos)
                  col (get-indent-column ced)]
              ;; Remove trailing whitespaces, if any.
              (when (and (pos? pos)
                         (space-s? (.getText doc (dec pos) 1)))
                (let [lcp (last-char pos)
                      beg (if lcp (inc (first lcp)) (start-of-line pos))
                      len (- pos beg)]
                  (when (pos? len)
                    (.remove doc beg len))))
              ;; Newline then indent.
              (.actionPerformed (DefaultEditorKit$InsertBreakAction.) event)
              (when-let [npos (indent ced col)]
                (.setCaretPosition ced npos)))))))))

(defn jump-to-matching-paren-pos-action
  []
  (proxy [AbstractAction] ["jump-to-matching-paren-pos"]
    (actionPerformed [event]
      (ced-action event
        (when-let [ppi (.getClientProperty ced *prop-ced-ppp-info*)]
          (let [[[pp0 pp1] _] ppi
                cpos (.getCaretPosition ced)]
            (if (= cpos pp0)
              (.setCaretPosition ced (inc pp1))
              (when (= (dec cpos) pp1)
                (.setCaretPosition ced pp0)))))))))

(defn select-by-matching-paren-pos-action
  []
  (proxy [AbstractAction] ["select-by-matching-paren-pos"]
    (actionPerformed [event]
      (ced-action event
        (when-let [ppi (.getClientProperty ced *prop-ced-ppp-info*)]
          (let [[[pp0 pp1] _] ppi
                cpos (.getCaretPosition ced)]
            (if (= cpos pp0)
              (.moveCaretPosition ced (inc pp1))
              (when (= (dec cpos) pp1)
                (.moveCaretPosition ced pp0)))))))))

(defn switch-pane-action
  []
  (proxy [AbstractAction] ["switch-pane"]
    (actionPerformed [event]
      (ced-action event
        (when-let [frm (.getTopLevelAncestor ced)]
          (let [mpl (.getComponent (.getContentPane frm) 0)
                cd1 (.ced1 mpl)
                cd2 (.ced2 mpl)
                spl (.splitter mpl)
                min (.getMinimumDividerLocation spl)
                loc (.getDividerLocation spl)
                max (.getMaximumDividerLocation spl)]
            (when (and (< min loc) (< loc max))
              (.requestFocusInWindow (if (identical? ced cd1) cd2 cd1)))))))))

(defn browse-api-action
  []
  (proxy [AbstractAction] ["browse-api"]
    (actionPerformed [event]
      (ced-action event
        (let [api-sym (or (.getSelectedText ced)
                          (fetch-symbol ced))]
          (when-not (empty? api-sym)
            (future (open-slix-with-args {:keyword api-sym} 'api-browser))))))))

(defn find-next-keyword-action
  []
  (proxy [AbstractAction] ["find-next-keyword"]
    (actionPerformed [event]
      (ced-action event
        (find-next-keyword ced (.getDocument ced) true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn invoke-opfile-with
  [ced action-fn]
  (let [doc (.getDocument ced)
        file (.getFile doc)]
    (if-let [mtch (re-matches (re-pattern (str "^" (get-src-path) "/(.*)\\.clj$")) (str file))]
      (let [fqsn (path2sym (second mtch))
            opsn (when-let [m (re-matches #"^slix\.(.*)$" (str fqsn))]
                   (second m))
            fsns (filter #(re-matches (re-pattern (str "^" % "(\\..*)*")) (str opsn)) (get-all-slix-sn))
            sn (when (seq fsns)
                 (reduce (fn [l n] (if (< (count (str l)) (count (str n))) n l)) nil fsns))]
        #_(lg "invoke-opfile-with fqsn:" fqsn "opsn:" opsn "sn:" sn)
        (action-fn fqsn sn))
      (popup-warning (.getTopLevelAncestor ced) "Not an sevenri/slix file"))))

(defn require-action
  []
  (proxy [AbstractAction] ["require"]
    (actionPerformed [event]
      (ced-action event
        (invoke-opfile-with ced
          (fn [fqsn sn]
            (future
              (let [ct (Thread/currentThread)
                    ccl (.getContextClassLoader ct)]
                (try
                  (.setContextClassLoader ct (if sn
                                               (create-slix-class-loader sn)
                                               ccl))
                  (require fqsn :reload)
                  (catch Exception e
                    (log-exception e fqsn))
                  (finally
                   (.setContextClassLoader ccl)))))))))))

(defn compile-action
  []
  (proxy [AbstractAction] ["compile"]
    (actionPerformed [event]
      (ced-action event
        (invoke-opfile-with ced
          (fn [fqsn sn]
            (future
              (try
                (binding [*compile-path* (str (get-sid-classes-path))]
                  (compile fqsn))
                (catch Exception e
                  (log-exception e fqsn))))))))))

(defn open-slix-action
  []
  (proxy [AbstractAction] ["open-slix"]
    (actionPerformed [event]
      (ced-action event
        (invoke-opfile-with ced
          (fn [fqsn sn]
            (try
              (when sn
                (open-slix sn))
              (catch Exception e
                (log-exception e fqsn)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn undo-action
  []
  (proxy [AbstractAction] ["undo"]
    (actionPerformed [event]
      (ced-action event
        (let [doc (.getDocument ced)
              pos (.createPosition doc (.getCaretPosition ced))]
          (when-let [udm (.getUndoMan doc)]
            (when (.canUndo udm)
              (let [oced (.getClientProperty ced *prop-ced-other-ced*)
                    navf (disable-caret-update oced)]
                (.undo udm)
                (.setCaretPosition ced (.getOffset pos))
                (enable-caret-update oced navf))))
          (.invokeDocWatcher doc))))))

(defn redo-action
  []
  (proxy [AbstractAction] ["redo"]
    (actionPerformed [event]
      (ced-action event
        (let [doc (.getDocument ced)
              pos (.createPosition doc (.getCaretPosition ced))]
          (when-let [udm (.getUndoMan doc)]
            (when (.canRedo udm)
              (let [oced (.getClientProperty ced *prop-ced-other-ced*)
                    navf (disable-caret-update oced)]
                (.redo udm)
                (.setCaretPosition ced (.getOffset pos))
                (enable-caret-update oced navf))))
          (.invokeDocWatcher doc))))))

(defn ced-notify-save
  "v := [file notify-fn]"
  [file]
  #_(lg "ced: ced-notify-save:" file)
  (doseq [sv (filter (fn [[_ [f nf]]] (and (= file f) (fn? nf)))
                     (xref-with :ced-notify-save))]
    (try
      (let [[_ [_ nf]] sv]
        (nf file))
      (catch Exception e
        (log-exception e)))))

(defn save-action
  []
  (proxy [AbstractAction] ["save"]
    (actionPerformed [event]
      (ced-action event
        (let [doc (.getDocument ced)
              file (.getFile doc)]
          (when (and (.exists file) (not (trash-path? file)))
            ;; FIX ME: need a better UI msg.
            (log-severe "ced: failed to trash file prior to save:" file))
          (.save doc)
          (ced-notify-save file))))))

(defn save-as-action
  []
  (proxy [AbstractAction] ["save-as"]
    (actionPerformed [event]
      (ced-action event
        (let [frm (.getTopLevelAncestor ced)
              dlg (FileDialog. frm "Save As Sevenri File" FileDialog/SAVE)
              doc (.getDocument ced)
              fpt (.getFile doc)
              fnm (if (instance? File fpt)
                    (.getName fpt)
                    "untitiled.clj")
              dir (if (instance? File fpt)
                    (.getParent fpt)
                    (str (get-user-path)))
              rpt #"(.*)\.clj$"
              flt (proxy [FilenameFilter] []
                    (accept [dir name]
                      (if (re-find rpt name)
                        true
                        false)))]
          (doto dlg
            (.setDirectory dir)
            (.setFile fnm)
            (.setFilenameFilter flt)
            (.show))
          (when-let [f (.getFile dlg)]
            (let [f-clj (if-let [m (re-find rpt f)]
                          (second m)
                          f)
                  fpath (File. (str (.replace (str (.getDirectory dlg) (sym2path f-clj)) \- \_) '.clj))
                  fpdir (.getParentFile fpath)
                  save? (if (and (.exists fpath) (trash-path? fpath))
                          true
                          (if (and (not (.exists fpdir)) (.mkdirs fpdir))
                            true
                            (if (.exists fpdir)
                              true
                              false)))]
              #_(lg "saving-as fpath:" fpath "fpdir:" fpdir "save?:" save?)
              (if save?
                (do
                  (.saveAs doc fpath)
                  (when-let [oplt (.getClientProperty ced *prop-ced-slix*)]
                    (invoke-later oplt #(update-title doc)))
                  (ced-notify-save fpath))
                ;; FIX ME: need a better warning.
                (log-warning "ced: failed to save as:" fpath)))))))))

(defn open-action
  []
  (proxy [AbstractAction] ["open"]
    (actionPerformed [event]
      (ced-action event
        (let [frm (.getTopLevelAncestor ced)
              dlg (FileDialog. frm "Open Sevenri File" FileDialog/LOAD)
              doc (.getDocument ced)
              fpt (.getFile doc)
              dir (if (instance? File fpt)
                    (.getParent fpt)
                    (str (get-user-path)))
              rpt #"(.*)\.clj$"
              flt (proxy [FilenameFilter] []
                    (accept [dir name]
                      (if (re-find rpt name)
                        true
                        false)))]
          (doto dlg
            (.setDirectory dir)
            (.setFilenameFilter flt)
            (.show))
          #_(lg "opening file:" (File. (.getDirectory dlg) (.getFile dlg)))
          (when-let [f (.getFile dlg)]
            (open-slix-with-args {:file (File. (.getDirectory dlg) f)} 'ced)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ced-actions
  []
  (into-array Action [(insert-tab-action)
                      (insert-break-action)
                      (jump-to-matching-paren-pos-action)
                      (select-by-matching-paren-pos-action)
                      (switch-pane-action)
                      (browse-api-action)
                      (find-next-keyword-action)
                      ;;
                      (require-action)
                      (compile-action)
                      (open-slix-action)
                      ;;
                      (undo-action)
                      (redo-action)
                      (save-action)
                      (save-as-action)
                      (open-action)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -getContentType
  [this]
  *content-type*)

(defn -getViewFactory
  [this]
  (proxy [ViewFactory] []
    (create [elem]
      (cview. elem))))

(defn -getActions
  [this]
  (TextAction/augmentList (.superGetActions this) (get-ced-actions)))

(defn -createDefaultDocument
  [this]
  (cdoc.))
