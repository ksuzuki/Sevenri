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

(ns oplix.ced.find
  (:use [openar log]
        [oplix.ced defs]
        [library.oplix.ced doclib])
  (:import (javax.swing.text Segment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initial-find-context
  []
  {:new-keyword nil
   :keyword ""
   :stale-content? true
   :matcher (re-matcher #"" "")
   :pos-history (list [0 nil])})

(defn set-find-keyword
  [fctxt keyword]
  (assoc fctxt :new-keyword keyword))

(defn content-is-stale
  [fctxt]
  (assoc fctxt :stale-content? true))

(defn set-find-start-pos
  [fctxt pos]
  (assoc fctxt :pos-history (list [pos nil])))

(defn get-find-start-pos
  [fctxt]
  (first (last (:pos-history fctxt))))

(defn create-match-pattern
  [keyword]
  (re-pattern (apply str (map #(if (Character/isUpperCase %)
                                 %
                                 (if (neg? (.indexOf "^\\.*?+&,|:=<=>-[](){}$" (int %)))
                                   (let [lc %
                                         uc (Character/toUpperCase %)]
                                     (if (= lc uc)
                                       lc
                                       (str "(" lc "|" uc ")")))
                                   (str "\\" %)))
                              keyword))))

(defn update-match-pattern-and-pos-history
  [fctxt]
  (if (nil? (:new-keyword fctxt))
    fctxt
    (let [newky (:new-keyword fctxt)
          oldky (:keyword fctxt)]
      (if (= newky oldky)
        (assoc fctxt
          :new-keyword nil)
        (let [newln (count newky)
              oldln (count oldky)
              phist (:pos-history fctxt)]
          (assoc fctxt
            :new-keyword nil
            :keyword newky
            :matcher (.usePattern (:matcher fctxt) (create-match-pattern newky))
            :pos-history (if (empty? newky)
                           (list (last phist))
                           (if (and (< newln oldln)
                                    (= newky (subs oldky 0 newln)))
                             (filter #(<= (count (last %)) newln) phist)
                             phist))))))))

(defn update-match-sequence
  [fctxt doc]
  (if (:stale-content? fctxt)
    (let [len (.getLength doc)
          seg (Segment.)]
      (.setPartialReturn seg true)
      (.getText doc 0 len seg)
      (let [seg (if (= (.length seg) len)
                  seg
                  (do
                    (.setPartialReturn seg false)
                    (.getText doc 0 len seg)
                    (assert (= (.length seg) len))
                    #_(lg "update-match-sequence: using exact seg")
                    seg))]
        (assoc fctxt
          :matcher (.reset (:matcher fctxt) (.subSequence seg 0 (dec len)))
          :stale-content? false)))
    fctxt))

(defn update-pos-history
  [fctxt doc pos keywd]
  (let [ph (cons [pos keywd] (:pos-history fctxt))]
    (.putProperty doc *prop-find-context* (assoc fctxt :pos-history ph))
    #_(lg "pos-history:" ph)
    pos))

(defn do-find
  [fctxt doc next?]
  (let [fctxt (-> fctxt
                  (update-match-pattern-and-pos-history)
                  (update-match-sequence doc))
        nkwd? (:new-keyword fctxt)
        keywd (:keyword fctxt)
        mtchr (:matcher fctxt)
        [s k] (first (:pos-history fctxt))
        [o _] (last (:pos-history fctxt))
        mxpos (dec (.getLength doc))]
    (.putProperty doc *prop-find-context* fctxt)
    (if (empty? keywd)
      [-1 o]
      (if (if (.hitEnd mtchr)
            (.find mtchr 0)
            (.find mtchr (min mxpos (if next? (+ s (count k)) s))))
        (let [pos (.start mtchr)]
          (update-pos-history fctxt doc pos keywd)
          [pos (count keywd)])
        [-1 o]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-find-ns-pattern
  []
  #"\((clojure-core/)?(in-)?ns\s+(\s*\^\{.*\}\s*)?[:']?([^\s()\"]+)")

(defn find-context-namespace
  "Parse the doc up to cpos and return the last namespace if found.
   Otherwise nil. when cpos is negative, parse the entire doc and return
   the first namespace found."
  ([ced]
     (find-context-namespace ced -1))
  ([ced cpos]
     (setup-doc-context ced nil
      (let [end (get-doc-context (if (neg? cpos) :max-pos cpos))]
        (setup-txt-segment 0 end
         (let [mtr (re-matcher (get-find-ns-pattern) "")]
           (loop [mtr (.reset mtr (.subSequence *txtseg* 0 end))
                  cns nil]
             (if (.hitEnd mtr)
               cns
               (if (.find mtr)
                 (let [s0 (.start mtr 0)
                       lcc0 (last-code-char s0)]
                   (if (and lcc0 (<= s0 (first lcc0)))
                     ;; not in comment
                     (let [s4 (.start mtr 4)
                           e4 (.end mtr 4)
                           g4 (str-at s4 (- e4 s4))]
                       (if (neg? cpos)
                         g4
                         (recur mtr g4)))
                     ;; in comment
                     (recur mtr cns)))
                 (recur mtr cns))))))))))
