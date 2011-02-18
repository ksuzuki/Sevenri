;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Openar, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns oplix.ced.init
  (:use [openar config core log oplix os ui utils]
        [oplix.ced defs listeners input2action utils])
  (:import (java.awt Dimension Font)
           (oplix.ced caret cdoc ckit)
           (oplix.ced.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-ced-frame
  []
  (let [frm (oplix-frame)
        cpn (.getContentPane frm)
        mpl (MainPanel.)
        cd1 (.ced1 mpl)
        cd2 (.ced2 mpl)
        mid (.modIndicator mpl)
        kit (ckit.)
        fnt (get-font)]
    ;; Setup ced1 and ced2, sharing the same kit and font.
    (doseq [ced [cd1 cd2]]
      (doto ced
        (.setEditorKitForContentType (.getContentType kit) kit)
        (.setContentType (.getContentType kit))
        (.setFont fnt)
        (.setEditable true)
        (map-input-to-action)
        (add-ced-listeners-and-properties mpl (if (identical? ced cd1) cd2 cd1))))
    (add-listeners mpl)
    ;;
    (doto cpn
      (.add mpl))
    (doto frm
      (.pack)
      (.setFont (Font. "Courier", 0, 12))
      (.setSize 640 400)
      (.setMinimumSize (Dimension. 320 200)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn process-args
  ([ced]
     (process-args ced (oplix-args)))
  ([ced args]
     (when-let [line (:line args)]
       (try
         (let [doc (.getDocument ced)
               lin (Integer/parseInt (str line))
               pos (.lineNumberToStartPosition doc lin)]
           (when-not (neg? pos)
             (goto-line-centered ced doc pos)))))))

(defn get-caret
  [frame ced]
  (if-let [cls (try
                 (cond
                  (is-mac?) (Class/forName "oplix.ced.aqcaret")
                  :else nil)
                 (catch Exception e
                   nil))]
    (try
      (cond
       (is-mac?) (.newInstance (first (.getConstructors cls))
                               (into-array Object [frame ced]))
       :else (caret.))
      (catch Exception e
        (caret.)))
    (caret.)))

(defn setup-doc-watcher
  "Currently this is used to update the mod indicator."
  [doc mod-indicator]
  (let [midupdater (fn []
                     (.setText mod-indicator (if (.isModified doc) "*" "")))
        dwatcher (fn []
                   (invoke-later *oplix* midupdater))]
    (.setDocWatcher doc dwatcher)))

(defn initial-setup
  []
  (let [frm (oplix-frame)
        cpn (.getContentPane frm)
        mpl (.getComponent cpn 0)
        spl (.splitter mpl)
        cd1 (.ced1 mpl)
        cd2 (.ced2 mpl)
        mid (.modIndicator mpl)
        lnc (.lineNumber mpl)
        fkw (.findKeyword mpl)
        doc (.getDocument (get-ced))]
    ;; Show the file name in the title. This also add the working file to
    ;; the xref.
    (update-title doc)
    ;; Install a custom caret for the current platform.
    (let [crt1 (get-caret frm cd1)]
      (.setBlinkRate crt1 500)
      (.setCaret cd1 crt1))
    (let [crt2 (get-caret frm cd2)]
      (doto crt2
        (.setBlinkRate 500)
        (.setAdjustVisibility false))
      (.setCaret cd2 crt2))
    ;; Remember *oplix* and enable double buffering and input methods.
    (doseq [cd [cd1 cd2]]
      (doto cd
        (.putClientProperty *prop-ced-oplix* *oplix*)
        (.setDoubleBuffered true)
        (.enableInputMethods true)))
    ;; Setup the doc watcher.
    (setup-doc-watcher doc mid)
    ;; Share the same doc between ced1 and ced2.
    (.setDocument cd2 doc)
    ;; Show only ced1 initially. Note: ced1 sits below ced2.
    (.setLastDividerLocation spl 0)
    (.setDividerLocation spl (double 0.0))
    (.requestFocusInWindow cd1)
    ;; Close the window with META+W.
    (doseq [c [cd1 cd2 lnc fkw]]
      (add-default-key-listener c)
      (add-ced-key-listener c frm doc))
    ;; Process args other than :file.
    (process-args cd1)))
