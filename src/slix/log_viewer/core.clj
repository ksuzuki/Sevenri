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

(ns slix.log-viewer.core
  (:require clojure.xml)
  (:use [clojure.java io]
        [sevenri config core log slix])
  (:import (java.awt Rectangle)
           (java.io BufferedReader BufferedWriter)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.io File FileInputStream FileOutputStream)
           (java.io InputStreamReader OutputStreamWriter)
           (java.text DateFormat)
           (java.util Date)
           (java.util.logging Filter Level SimpleFormatter StreamHandler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn copy-temp-log-file
  []
  (let [lsrc (get-sevenri-log-file)
        lisr (InputStreamReader. (FileInputStream. lsrc) "UTF-8")
        ltgt (get-sid-temp-file "log" "xml")
        losw (OutputStreamWriter. (FileOutputStream. ltgt) "UTF-8")]
    (with-open [lrdr (BufferedReader. lisr)
                lwtr (BufferedWriter. losw)]
      (loop [lseq (line-seq lrdr)]
        (if (seq lseq)
          (let [line (first lseq)]
            (.write lwtr line 0 (count line))
            (.newLine lwtr)
            (recur (next lseq)))
          (do
            (.write lwtr "</log>" 0 6)
            (.newLine lwtr)))))
    ltgt))

(defn copy-temp-dtd-file
  "The destination name has to be 'logger.dtd' because that's what's
   expected in the log.xml."
  []
  (let [dsrc (get-resources-path (get-config 'src.resources.logger.dir-name)
                                 (get-config 'src.resources.logger.dtd-file-name))
        dtgt (get-sid-temp-path (get-config 'src.resources.logger.dtd-file-name))]
    (copy dsrc dtgt :econdig "UTF-8")
    dtgt))

(defn copy-temp-logger-files
  []
  (when (.exists (get-sevenri-log-file))
    [(copy-temp-log-file) (copy-temp-dtd-file)]))

(defn delete-temp-logger-files
  [log-dtd]
  (let [[log dtd] log-dtd]
    (when (.exists log)
      (.delete log))
    (when (.exists dtd)
      (.delete dtd))))

(defn read-log-file
  []
  (if-let [log-dtd (copy-temp-logger-files)]
    (let [[log dtd] log-dtd
          lxml (clojure.xml/parse log)
          sbuf (StringBuffer.)
          dfmt (DateFormat/getDateInstance DateFormat/MEDIUM)
          tfmt (DateFormat/getTimeInstance DateFormat/MEDIUM)]
      (delete-temp-logger-files log-dtd)
      ;;
      (doseq [rec-elem (:content lxml)]
        (let [[date-elem millis-elem sequence-elem
               logger-elem level-elem class-elem
               method-elem thread-elem message-elem] (:content rec-elem)
              ;;
              date (Date. (Long/parseLong (first (:content millis-elem))))
              logger (first (:content logger-elem))
              method (first (:content method-elem))
              level (first (:content level-elem))
              message (first (:content message-elem))]
          (doto sbuf
            (.append (str (.format dfmt date) " "))
            (.append (str (.format tfmt date) " "))
            (.append (str logger " "))
            (.append (str method "\n"))
            (.append (str level ": "))
            (.append (str message "\n")))))
      ;;
      (.toString sbuf))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-view-log
  []
  (let [fr (slix-frame)
        cp (.getContentPane fr)
        sp (.getComponent cp 0)
        vp (.getViewport sp)
        ta (.getView vp)
        os (ByteArrayOutputStream.)
        fl (proxy [Filter][] (isLoggable [r] true))
        sh (StreamHandler. os (SimpleFormatter.))
        ag (agent [*slix* os ta vp sh])]
    (doto sh
      (.setLevel Level/INFO)
      (.setFilter fl))
    (.setText ta (read-log-file))
    (when-let [sevenri-logger (get-sevenri-logger)]
      (.addHandler sevenri-logger sh)
      (send ag (fn *fn* [[op os ta vp sh]]
                 (if (get-slix (slix-name op))
                   ;; If this viewer is still open...
                   (do
                     (.flush sh)
                     ;; If there is something in the stream, write it to
                     ;; the text area.
                     (when (pos? (.size os))
                       (let [s (.toString os)]
                         (.reset os)
                         (invoke-later op #(let [doc (.getDocument ta)
                                                 rot (.getDefaultRootElement doc)
                                                 lec (max 0 (dec (.getElementCount rot)))]
                                             ;; Got the last element count/index.
                                             ;; Then append the new string/elements.
                                             (.append ta s)
                                             ;; Get the top of the new elements, which
                                             ;; is indexed by lec. Then scroll to the
                                             ;; element start offset.
                                             (let [elm (.getElement (.getDefaultRootElement doc) lec)
                                                   rec (.modelToView ta (.getStartOffset elm))
                                                   vrc (.getViewRect vp)
                                                   nrc (Rectangle. (.x rec) (.y rec) (.width vrc) (.height vrc))]
                                               (.scrollRectToVisible ta nrc)
                                               (.setCaretPosition ta (.getStartOffset elm)))))))
                     ;; Wait a sec.
                     (Thread/sleep 1000)
                     ;; Perform this fn again.
                     (send *agent* *fn*)
                     ;; return the fn args back for the next call.
                     [op os ta vp sh])
                   ;; This viewer seems closed. Remove the streamHandler.
                   (when-let [sevenri-logger (get-sevenri-logger)]
                     (.removeHandler sevenri-logger sh))))))))
