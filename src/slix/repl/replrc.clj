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

;; Load required

(use '[clojure pprint]
     '[clojure.java browse io]
     '[clojure.java.shell :rename {sh cjsh}]
     '[sevenri debug event props utils]
     '[slix.repl.core :only (repl clear-repl-content)])

;; fns for repl

(defmacro crc
  []
  `(clear-repl-content))

(defn do-replrc*
  [fnc]
  (let [replrc (get-sid-slix-path (slix-sn) 'replrc!clj)]
    (when (.exists replrc)
      (fnc (str replrc)))))

(defn load-replrc
  []
  (do-replrc* load-file))

(defn ced-replrc
  []
  (do-replrc* #(open-slix-with-args {:file %} 'ced)))

(defn restore-replrc
  ([]
     (restore-replrc (slix-sn)))
  ([sn]
     (let [src (slix.repl.core/get-startup-script-file)
           tgt (get-sid-slix-path sn (slix.repl.core/get-startup-script-file-name))]
       (trash-path? tgt)
       (clojure.java.io/copy src tgt))))

;; slix fns and macros

(defmacro opop [o] `(open-slix '~o))
(defmacro svop [n] `(save-slix '~n))
(defmacro clop [n] `(close-slix '~n))

(defn print-cl
  ([]
     (print-cl *slix*))
  ([slix]
     (when (is-slix? slix)
       (letfn [(pcl [cl]
                 (loop [cl cl]
                   (when cl
                     (println cl)
                     (print-seq (seq (.getURLs cl)))
                     (recur (.getParent cl)))))]
         (pcl (slix-cl slix))))))

;; utility fns and macros

(defmacro ced
  ([]
     `(open-slix ~''ced))
  ([tgt]
     (cond
      (symbol? tgt)
        `(open-slix-with-args {:file '~tgt} ~''ced)
      (or (string? tgt) (instance? java.io.File (eval tgt)))
        `(open-slix-with-args {:file ~tgt} ~''ced)
      (and (map? tgt) (:file tgt))
        `(open-slix-with-args ~tgt ~''ced)
      (var? (eval tgt))
        (let [vr# (eval tgt) vm# (meta vr#)]
          (when (:file vm#)
            (if (is-sevenri-var? vr#)
              (let [f# (ns-name (:ns vm#)) l# (:line vm#)]
                `(open-slix-with-args {:file '~f# :line ~l#} ~''ced))
              `(open-slix-with-args @(get-code-of ~tgt) ~''ced))))
      :else
        `(open-slix ~''ced)))
  ([file line]
     (cond
      (symbol? file)
      `(open-slix-with-args {:file '~file :line ~line} ~''ced)
      (or (string? file) (instance? java.io.File file))
      `(open-slix-with-args {:file ~file :line line} ~''ced)
      :else
      `(open-slix ~''ced))))

(defn lv
  []
  (open-slix 'log-viewer))

(defmacro ps
  [sq]
  `(print-seq ~sq))

(defmacro sh
  [cmd & args]
  `(with-sh-dir (get-user-path)
     (let [r# (apply cjsh (str ~cmd) (map str (list ~@args)))]
       (println (:out r#))
       (when-not (empty? (:err r#))
         (println (:err r#))))))

(defmacro cat
  [file]
  `(sh "cat" ~file))

(defmacro ls
  "Emulate ls command. Default path is 'user.dir'.
   ls options should begin with '-' but no need to quote.
   File args can be either string, File object, or quoted symbol.
   Depends on REPL's property 'console."
  [& args]
  ;; Convert the ls options arg into string if any.
  (let [args (if (= (first (str (first args))) \-)
               (apply list (str (first args)) (rest args))
               args)]
    `(let [argvals# [~@args]
           options# (when (= (first (str (first argvals#))) \-) (first argvals#))
           longout# (and options# (re-find #"l" options#)) ;; detect long output
           pathlst# (or (seq (if options# (rest argvals#) argvals#))
                        (list (get-user-path)))]
       #_(println "argvals:" argvals# "options:" options# "longout:" longout# "pathlst:" pathlst#)
       (doseq [path# pathlst#]
         (let [abpath# (if (instance? java.io.File path#)
                         path#
                         (java.io.File. (sym2path (obj2sym path#))))
               pathnm# (if (.isAbsolute abpath#)
                         (str abpath#)
                         (str (java.io.File. (get-user-path) (str abpath#))))
               result# (apply cjsh "ls" (filter identity [options# pathnm# :out-enc "UTF-8"]))]
           ;; Print target path and the sh result.
           (println (str path# ":"))
           (if (and (zero? (:exit result#)) (empty? (:err result#)))
             (let [items# (seq (.split (:out result#) "\n"))]
               (if longout#
                 ;; Print items per-line
                 (do
                   (apply println (interpose "\n" items#))
                   (println))
                 ;; Print items in multiple columns, like 'ls' does.
                 (let [itc# (count items#)
                       ;; Find longest item and its length.
                       mxa# (reduce (fn [a# s#]
                                      (let [cs# (count s#)]
                                        (if (< (second a#) cs#)
                                          [s# cs#]
                                          a#)))
                                    ["" 0]
                                    items#)
                       ;; Dig out FontMetrics.
                       txp# (.getTextPane (get-prop (slix-props) ~''console))
                       fmx# (.getFontMetrics txp# (.getFont txp#))
                       ;; Figure out required columns and rows. OK to use
                       ;; the width of the textpane because it's mapped to
                       ;; its viewport 1:1.
                       mxw# (.stringWidth fmx# (str (first mxa#) "  "))
                       clm# (int (Math/floor (/ (.getWidth txp#) mxw#)))
                       row# (int (Math/ceil (/ itc# clm#)))
                       ;; Format each item in left-justified and space padded
                       ;; fixed size, preceeded by \newline optionally.
                       fmt# (str "%s%-" (inc (second mxa#)) "s")
                       its# (for [r# (range row#) c# (range clm#)]
                              (let [brk# (if (and (pos? r#) (zero? c#)) "\n" "")
                                    idx# (+ r# (* c# row#))
                                    itm# (if (< idx# itc#) (nth items# idx#) "")]
                                (format fmt# brk# itm#)))]
                   #_(println "mxa:" mxa# "clm:" clm# "row:" row#)
                   (apply print its#)
                   (println "\n"))))
             ;; 'sh ls' failed.
             (println "ERR:" (:err result#))))))))

(defn open-sevenri-users-manual
  []
  (browse-url (str "file://"
                   (file (get-doc-path 'Manuals.Sevenri-Users-Manual.out)
                         "Table_of_Contents.html"))))

(defmacro open-sum
  []
  `(open-sevenri-users-manual))

;; fns and macros for development

(defmacro reload
  [libsym]
  (let [libsym# (symbol (str "slix." libsym))]
    `(require '~libsym# :reload)))

(defmacro browse-api
  [kwd]
  `(open-slix-with-args {:keyword (str '~kwd)} '~'api-browser))

(defmacro ba
  [kwd]
  `(browse-api ~kwd))

(defn do-frame-xml*
  [sn name func]
  (let [fx (get-sid-slix-frame-file sn name)]
    (when (.exists fx)
      (func fx))))

(defn browse-frame-xml
  [sn name]
  (do-frame-xml* sn name (fn [f] (browse-url (str "file:///" f)))))

(defmacro bfx
  [sn name]
  `(browse-frame-xml ~sn ~name))

(defn cat-frame-xml
  [sn name]
  (do-frame-xml* sn name (fn [f] (sh "cat" f))))

(defmacro cfx
  [sn name]
  `(cat-frame-xml ~sn ~name))

(defmacro lein
  [& args]
  (let [args# (map str args)]
    `(sh "lein" ~@args#)))

(defmacro git
  [& args]
  (let [args# (map str args)]
    `(sh "git" ~@args#)))

(defmacro gs
  []
  `(git "status"))

(defmacro gb
  []
  `(git "branch"))
