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
     '[clojure.java.shell :rename {sh cjsh}])

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

(defn ls
  [root]
  (let [rep (re-pattern (str root "/[^/]*"))]
    (ps (find-files #(re-matches rep (str %)) root))))

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
