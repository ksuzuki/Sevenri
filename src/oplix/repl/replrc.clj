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
  (let [replrc (get-dop-oplix-file (oplix-on) 'replrc)]
    (when (.exists replrc)
      (fnc (str replrc)))))

(defn load-replrc
  []
  (do-replrc* load-file))

(defn ced-replrc
  []
  (do-replrc* #(open-oplix-with-args {:file %} 'ced)))

(defn restore-replrc
  ([]
     (restore-replrc (oplix-on)))
  ([on]
     (let [src (oplix.repl.core/get-startup-script-file)
           tgt (get-dop-oplix-file on (oplix.repl.core/get-startup-script-file-name))]
       (trash-file? tgt)
       (clojure.java.io/copy src tgt))))

;; oplix fns and macros

(defmacro opop [o] `(open-oplix '~o))
(defmacro svop [n] `(save-oplix '~n))
(defmacro clop [n] `(close-oplix '~n))

(defn print-cl
  ([]
     (print-cl *oplix*))
  ([oplix]
     (when (is-oplix? oplix)
       (letfn [(pcl [cl]
                 (loop [cl cl]
                   (when cl
                     (println cl)
                     (print-seq (seq (.getURLs cl)))
                     (recur (.getParent cl)))))]
         (pcl (oplix-cl oplix))))))

;; utility fns and macros

(defmacro ced
  ([]
     `(open-oplix ~''ced))
  ([tgt]
     (cond
      (symbol? tgt)
        `(open-oplix-with-args {:file '~tgt} ~''ced)
      (or (string? tgt) (instance? java.io.File (eval tgt)))
        `(open-oplix-with-args {:file ~tgt} ~''ced)
      (and (map? tgt) (:file tgt))
        `(open-oplix-with-args ~tgt ~''ced)
      (var? (eval tgt))
        (let [vr# (eval tgt) vm# (meta vr#)]
          (when (:file vm#)
            (if (is-openar-var? vr#)
              (let [f# (ns-name (:ns vm#)) l# (:line vm#)]
                `(open-oplix-with-args {:file '~f# :line ~l#} ~''ced))
              `(open-oplix-with-args @(get-code-of ~tgt) ~''ced))))
      :else
        `(open-oplix ~''ced)))
  ([file line]
     (cond
      (symbol? file)
      `(open-oplix-with-args {:file '~file :line ~line} ~''ced)
      (or (string? file) (instance? java.io.File file))
      `(open-oplix-with-args {:file ~file :line line} ~''ced)
      :else
      `(open-oplix ~''ced))))

(defn lv
  []
  (open-oplix 'log-viewer))

(defmacro ps
  [sq]
  `(print-seq ~sq))

(defmacro sh
  [cmd & args]
  `(with-sh-dir (get-user-dir)
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

;; fns and macros for development

(defmacro reload
  [libsym]
  (let [libsym# (symbol (str "oplix." libsym))]
    `(require '~libsym# :reload)))

(defmacro browse-api
  [kwd]
  `(open-oplix-with-args {:keyword (str '~kwd)} '~'api-browser))

(defmacro ba
  [kwd]
  `(browse-api ~kwd))

(defn do-frame-xml*
  [on name func]
  (let [fx (get-dop-oplix-frame-file on name)]
    (when (.exists fx)
      (func fx))))

(defn browse-frame-xml
  [on name]
  (do-frame-xml* on name (fn [f] (browse-url (str "file:///" f)))))

(defmacro bfx
  [on name]
  `(browse-frame-xml ~on ~name))

(defn cat-frame-xml
  [on name]
  (do-frame-xml* on name (fn [f] (sh "cat" f))))

(defmacro cfx
  [on name]
  `(cat-frame-xml ~on ~name))

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
