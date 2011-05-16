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

(ns slix.exceptor.core
  (:use [sevenri core log slix utils])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *edb* (agent {})) ;; a simple excpetion database

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-edb
  []
  (send *edb* (fn [edb] {})))

(defn add-to-edb
  "Avoid opening multiple exceptors for the same file/line."
  [^Exception e file line open-ced?]
  (send *edb* (fn [edb]
                (let [launch? (if-let [l (get edb (str file))]
                                (not= l line)
                                true)]
                  (if launch?
                    (let [args (if open-ced?
                                 {:exception e :file file :line line :open-ced true}
                                 {:exception e :file file :line line})]
                      #_(lg "add-to-edb: args:" args)
                      (open-slix-with-args args 'exceptor (gensym "Exceptor"))
                      (assoc edb (str file) line))
                    edb)))))

(defn remove-from-edb
  [file line]
  (send *edb* (fn [edb] (dissoc edb (str file) line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-launch
  ([^Exception e file line]
     (do-launch e file line true))
  ([^Exception e file line open-ced?]
     (add-to-edb e file line open-ced?)))

(defn launch-exceptor
  [^Exception e fqsn file-name line-number]
  #_(lg "launch-exceptor: fqsn:" fqsn "file-name:" file-name "line-number:" line-number)
  (if (re-find (re-pattern (str (get-slix-ns 'ced))) (str fqsn))
    ;; Cannot open ced when it's the cause of the exception.
    (do-launch e (File. file-name) line-number false)
    (let [clj-file (get-src-path (str (sym2path fqsn) '.clj))
          sub-path (sym2path fqsn)]
      #_(lg "launch-exceptor: clj-file:" clj-file "sub-path:" sub-path)
      (if (and (= (.getName clj-file) file-name) (.exists clj-file))
        (do-launch e clj-file line-number)
        (let [clj-file (File. (get-src-path) (str sub-path "/" file-name))]
          (if (.exists clj-file)
            (do-launch e clj-file line-number)
            (let [sp (seq (.split sub-path "/"))]
              (if (< 2 (count sp)) ;; e.g. slix/x/y
                (let [sub-path (apply str (butlast (interleave (butlast sp) (repeat "/"))))
                      clj-files (find-files '.clj (File. (get-src-path) sub-path))
                      cand-clj-files (filter #(= file-name (.getName %)) clj-files)]
                  (if (seq cand-clj-files)
                    (let [f (first cand-clj-files)]
                      ;; FIX ME; what if multiple files with the same name found?
                      (do-launch e f line-number))
                    (do-launch e (File. file-name) line-number false)))
                (do-launch e (File. file-name) line-number false)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open-ced-if-requested
  []
  #_(lg "slix-args:" (slix-args))
  (when (:open-ced (slix-args)) ;; See above for :open-ced.
    (invoke-later #(do
                     (deref (open-slix-with-args
                             {:file (:file (slix-args))
                              :line (:line (slix-args))}
                             'ced))
                     ;; Make sure to make this window comes on the front.
                     (.toFront (slix-frame))))))

(defn close-exceptor
  []
  (remove-from-edb (:file (slix-args)) (:line (slix-args))))
