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

(ns slix.ced.cdoc
  (:gen-class
   :extends javax.swing.text.PlainDocument
   :post-init initFindContext*
   :exposes-methods {insertString superInsertString remove superRemove}
   :methods [[initProperties [java.io.File java.lang.String javax.swing.undo.UndoManager] java.lang.Void]
             [setFile [java.io.File] java.lang.Void]
             [getFile [] java.io.File]
             [getFileName [] java.lang.String]
             [getUndoMan [] javax.swing.undo.UndoManager]
             [setDocWatcher [clojure.lang.Fn] java.lang.Void]
             [invokeDocWatcher [] java.lang.Void]
             [modified [java.lang.Boolean] java.lang.Void]
             [isModified [] java.lang.Boolean]
             [save [] java.lang.Void]
             [saveAs [java.io.File] java.lang.Void]
             [lineNumberToStartPosition [java.lang.Integer] java.lang.Integer]
             [positionToLineNumber [java.lang.Integer] java.lang.Integer]
             [initFindContext [] java.lang.Void]
             [setFindKeyword [java.lang.String] java.lang.Void]
             [setFindStartPos [java.lang.Integer] java.lang.Void]
             [getFindStartPos [] java.lang.Integer]
             [find [java.lang.Boolean] clojure.lang.PersistentVector]]
   :main false)
  (:use [sevenri config core log utils]
        [slix.ced defs find])
  (:import (java.io File FileOutputStream OutputStreamWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -initProperties
  [this file encoding undoman]
  (.putProperty this *prop-file* file)
  (.putProperty this *prop-file-encoding* encoding)
  (.putProperty this *prop-undoman* undoman))

(defn -setFile
  [this file]
  (.putProperty this *prop-file* file))

(defn -getFile
  [this]
  (.getProperty this *prop-file*))

(defn -getFileName
  [this]
  (when-let [f (.getProperty this *prop-file*)]
    (let [cfp (str f)
          rpt (str "^"
                   (get-src-dir)
                   "/(("
                   (apply str (interleave (get-sevenri-namespaces) (repeat \|)))
                   ")/.*)\\.clj$")
          rfd (re-find (re-pattern rpt) cfp)]
      (if rfd
        (str (path2nssym (second rfd)))
        (.getName f)))))

(defn -getUndoMan
  [this]
  (.getProperty this *prop-undoman*))

(defn -setDocWatcher
  [this dwatcher]
  (.putProperty this *prop-docwatcher* dwatcher))

(defn -invokeDocWatcher
  [this]
  (when-let [dwatcher (.getProperty this *prop-docwatcher*)]
    (dwatcher)))

(defn -isModified
  [this]
  (if-let [undoman (.getUndoMan this)]
    (let [svdedit (.getProperty this *prop-savededit*)
          modified? (if svdedit
                      (if (identical? svdedit (.superEditToBeUndone undoman))
                        false
                        true)
                      (if (.canUndo undoman)
                        true
                        false))]
      modified?)
    false))

(defn -save
  [this]
  (when-let [file (.getFile this)]
    (when (or (.isModified this) (not (.exists file)))
      (try
        ;;
        (with-open [fotpt (FileOutputStream. file)
                    oswtr (OutputStreamWriter. fotpt (.getProperty this *prop-file-encoding*))]
          (.write oswtr (.getText this 0 (.getLength this)) 0 (.getLength this)))
        ;;
        (when-let [undoman (.getUndoMan this)]
          (let [svdedit (.superEditToBeUndone undoman)]
            (.putProperty this *prop-savededit* svdedit)))
        (.invokeDocWatcher this)
        ;;
        (catch Exception e
          (log-exception e))))))

(defn -saveAs
  [this file]
  (when (instance? File file)
    (try
      (with-open [fotpt (FileOutputStream. file)
                  oswtr (OutputStreamWriter. fotpt (.getProperty this *prop-file-encoding*))]
        (.write oswtr (.getText this 0 (.getLength this)) 0 (.getLength this)))
      ;;
      (when-let [undoman (.getUndoMan this)]
        (let [svdedit (.superEditToBeUndone undoman)]
          (.putProperty this *prop-savededit* svdedit)))
      (.invokeDocWatcher this)
      ;;
      (catch Exception e
        (log-exception e)))
    (.setFile this file)))

(defn -lineNumberToStartPosition
  [this line]
  (let [relm (.getDefaultRootElement this)
        lmax (.getElementCount relm)]
    (if (and (pos? line) (<= line lmax))
      (let [elm (.getElement relm (dec line))]
        (.getStartOffset elm))
      -1)))

(defn -positionToLineNumber
  [this pos]
  (let [relm (.getDefaultRootElement this)
        emax (.getElementCount relm)
        pmax (.getEndOffset (.getElement relm (dec emax)))]
    (if (and (<= 0 pos) (<= pos pmax))
      (inc (.getElementIndex relm pos))
      -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -initFindContext*
  [this & _]
  (.putProperty this *prop-find-context* (initial-find-context)))
  
(defn -initFindContext
  [this]
     (-initFindContext* this))
  
(defn -setFindKeyword
  [this keyword]
  (when-let [fctxt (.getProperty this *prop-find-context*)]
    (.putProperty this *prop-find-context* (set-find-keyword fctxt keyword))))

(defn -setFindStartPos
  [this pos]
  (when-let [fctxt (.getProperty this *prop-find-context*)]
    (.putProperty this *prop-find-context* (set-find-start-pos fctxt pos))))

(defn -getFindStartPos
  [this]
  (when-let [fctxt (.getProperty this *prop-find-context*)]
    (get-find-start-pos fctxt)))

(defn -find
  [this next?]
  (when-let [fctxt (.getProperty this *prop-find-context*)]
    (do-find fctxt this next?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn content-is-modified
  [this]
  (doto this
    (.putProperty *prop-find-context*
                  (content-is-stale (.getProperty this *prop-find-context*)))
    (.invokeDocWatcher)))

(defn -insertString
  [this offs str a]
  (.superInsertString this offs str a)
  (content-is-modified this))

(defn -remove
  [this offs len]
  (.superRemove this offs len)
  (content-is-modified this))
