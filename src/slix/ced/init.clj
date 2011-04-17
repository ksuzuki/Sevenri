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

(ns slix.ced.init
  (:use [sevenri config core log os slix ui]
        [slix.ced defs listeners input2action ui])
  (:import (java.awt Dimension Font)
           (java.io File FileInputStream InputStreamReader StringReader)
           (slix.ced caret cdoc ckit undoman)
           (slix.ced.gui MainPanel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ced
  ([]
     (get-ced -1))
  ([n]
     (when *slix*
       (let [frm (slix-frame)
             cpn (.getContentPane frm)
             mpl (.getComponent cpn 0)]
         (if (neg? n)
           (if-let [ced (.getClientProperty mpl *prop-mp-last-ced*)]
             ced
             (.ced1 mpl))
           (cond
            (= n 1) (.ced1 mpl)
            :else (.ced2 mpl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-tab-size-spaces
  []
  (apply str (repeat *tab-size* \space)))

(defn get-untabbing-string-reader
  [file]
  (let [stbuffer (StringBuffer.)]
    (with-open [fistream (FileInputStream. file)
                isreader (InputStreamReader. fistream *file-encoding*)]
      (loop [col 0]
        (when (.ready isreader)
          (let [c (.read isreader)]
            (cond
             (= c (int \tab)) (let [n (mod col *tab-size*)]
                                (.append stbuffer (subs (get-tab-size-spaces) n))
                                (recur (+ col (- *tab-size* n))))
             (= c (int \newline)) (do
                                    (.append stbuffer (char c))
                                    (recur 0))
             :else (do
                     (.append stbuffer (char c))
                     (recur (inc col))))))))
    (StringReader. (.toString stbuffer))))

(defn get-ced-file
  "Return a File object specified by a file specifier. The specifer is
   either a symbol, a string, or a File object.
   * When it's a symbol, the sym2path translation is applied to it and then
     a file in the src or src/slix directories is looked up, depending on
     the prefix in the specifier.
   * When it's a string, it should be either an absolute path or a relative
     path from the current Sevenri directory.
   * When it's a File object, it's taken as is.
   Eithe way the .clj extension will be added to the specifier if missing.
   The default scratch File object will be returned when the specified file
   doesn't exist."
  ([]
     (get-ced-file (:file (slix-args))))
  ([file]
     (let [tlns (interleave (get-sevenri-namespaces) (repeat \|))
           rptn (re-pattern (str "^(" (apply str (butlast tlns)) ")[./].*"))
           path (cond
                 (symbol? file) (if (re-matches rptn (str file))
                                  (get-src-path file)
                                  (get-slix-path file))
                 (string? file) (if (.isAbsolute (File. file))
                                  (File. file)
                                  (File. (get-user-path) file))
                 (instance? File file) file
                 :else nil)
           fclj (when path
                  (if (re-find #"\.clj$" (str path))
                    path
                    (File. (str path '.clj))))]
       ;; Fallback to the default scratch file when the specified file
       ;; doesn't exist.
       (if (and fclj (.exists (.getParentFile fclj)))
         fclj
         (get-library-path 'user (get-config 'src.library.user.scratch-file-name))))))

(defn load-ced-file
  []
  (let [fclj (get-ced-file)]
    (when (and fclj (.exists fclj))
      (.read (get-ced) (get-untabbing-string-reader fclj) nil))
    ;;
    (let [doc (.getDocument (get-ced))
          udm (undoman.)]
      ;; Setup undoman and properties for doc.
      (.setLimit udm -1) ;; no undo limit
      (.addUndoableEditListener doc udm)
      (.initProperties doc fclj *file-encoding* udm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-ced-frame
  []
  (let [frm (slix-frame)
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
        (.setForeground *foreground-color*)
        (.setBackground *background-color*)
        (.setCaretColor *caret-color*)
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

;;;;

(defn process-args
  ([ced]
     (process-args ced (slix-args)))
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
                  (is-mac?) (Class/forName "slix.ced.aqcaret")
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
                   (invoke-later *slix* midupdater))]
    (.setDocWatcher doc dwatcher)))

(defn initial-setup
  []
  (let [frm (slix-frame)
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
    ;; Remember *slix* and enable input methods.
    (doseq [cd [cd1 cd2]]
      (doto cd
        (.putClientProperty *prop-ced-slix* *slix*)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn list-all-action-mappings
  []
  (when-let [am (.getActionMap (get-ced))]
    (let [ams (map #(.toString %) (seq (.allKeys am)))]
      (sort (proxy [java.util.Comparator][]
              (compare [k1 k2] (.compareTo k1 k2)))
            ams))))

(defn list-all-input-mappings
  []
  (when-let [im (.getInputMap (get-ced))]
    (letfn [(to-hrkey [ks]
              (.replaceAll (.replace (.toUpperCase (.toString ks)) "PRESSED " "") " " "+"))]
      (let [kms (map (fn [ks] [(to-hrkey ks) (.get im ks)]) (seq (.allKeys im)))]
        (sort (proxy [java.util.Comparator][]
                (compare [k1 k2] (.compareTo (first k1) (first k2))))
              kms)))))
