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

(ns slix.ced.utils
  (:use [sevenri config core log slix utils]
        [slix.ced defs])
  (:import (java.awt Dimension Font)
           (java.io File FileInputStream InputStreamReader StringReader)
           (slix.ced undoman)))

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

(defn get-file
  "Return a File object specified by a file specifier. The specifer is
   either a symbol, a string, or a File object.
   When it's a symbol, period to slash and hypen to underscore translations
   are applied to it and then a file in the src or src/slix directories is
   looked up, depending on the prefix in the specifier.
   When it's a string, it should be an absolute path or a relative path from
   the current Sevenri directory.
   When it's a File object, it's taken as is.
   Eithe way the .clj extension will be added to the specifier if missing,
   and the default scratch File object will be returned when the specified
   file doesn't exist."
  ([]
     (get-file (:file (slix-args))))
  ([afile]
     (let [rptn (re-pattern
                 (str "^(" (apply str (butlast (interleave (get-sevenri-namespaces) (repeat \|)))) ")[./].*"))
           fpath (cond
                  (symbol? afile) (let [file (str afile)]
                                    (if (re-matches rptn file)
                                      (File. (get-src-dir) (nssym2path file))
                                      (File. (get-slix-dir) (nssym2path file))))
                  (string? afile) (if (.isAbsolute (File. afile))
                                      (File. afile)
                                      (File. (get-user-dir) afile))
                  (instance? File afile) afile
                  :else nil)
           fpclj (when fpath
                   (if (re-find #"\.clj$" (str fpath))
                     fpath
                     (File. (str fpath ".clj"))))]
       ;; Fallback to the default scratch file when the specified file
       ;; doesn't exist.
       (if (and fpclj (.exists (.getParentFile fpclj)))
         fpclj
         (File. (get-library-dir 'user) (str (get-default :src :library :user :scratch-file-name)))))))

(defn load-file*
  []
  (let [fpclj (get-file)]
    (when (and fpclj (.exists fpclj))
      (.read (get-ced) (get-untabbing-string-reader fpclj) nil))
    ;;
    (let [doc (.getDocument (get-ced))
          udm (undoman.)]
      ;; Setup undoman and properties for doc.
      (.setLimit udm -1) ;; no undo limit
      (.addUndoableEditListener doc udm)
      (.initProperties doc fpclj *file-encoding* udm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-font
  []
  (let [lge (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
        pref (filter #(= (ffirst *preferred-fonts*) %)
                     (map str (seq (.getAvailableFontFamilyNames lge))))
        [name style size] (if (seq pref)
                             (first *preferred-fonts*)
                             (second *preferred-fonts*))]
    (Font. name style size)))

(defn update-title
  "Should be called in the *slix* context."
  [doc]
  (add-to-xref *slix* :ced-file (.getFile doc))
  (let [fname (.getFileName doc)
        fnlen (count fname)
        fnstr (.substring fname 0 (if (re-find #"\.clj$" fname) (- fnlen 4) fnlen))
        title (str (slix-name) " - " (path2nssym fnstr))]
    (set-slix-title title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn list-all-action-mappings
  ([]
     (when-let [ced (get-ced)]
       (list-all-action-mappings (.getActionMap ced))))
  ([am]
     (when am
       (let [ams (map #(.toString %) (seq (.allKeys am)))]
         (sort (proxy [java.util.Comparator][]
                 (compare [k1 k2] (.compareTo k1 k2)))
               ams)))))

(defn list-all-input-mappings
  ([]
     (when-let [ced (get-ced)]
       (list-all-input-mappings (.getInputMap ced))))
  ([im]
     (when im
       (letfn [(to-hrkey [ks]
                         (.replaceAll (.replace (.toUpperCase (.toString ks)) "PRESSED " "") " " "+"))]
         (let [kms (map (fn [ks] [(to-hrkey ks) (.get im ks)]) (seq (.allKeys im)))]
           (sort (proxy [java.util.Comparator][]
                   (compare [k1 k2] (.compareTo (first k1) (first k2))))
                 kms))))))
