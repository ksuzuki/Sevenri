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

(ns library.oplix.ced.paren
  (:use [openar log]
        [library.oplix.ced doclib]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-mparen
  "The algo is simple. See each line is consisted of code text followed by
   possible comment text. Both code and comment text may also contain strings.
   While Keep tracking parens in code, tracking parens in string and comment
   can be done independently and dicardable at newline, except for string
   started in code text."
  ([prnpos prnchr forward?]
     (let [begpos (or (get-doc-context :beg-pos)
                      (top-level-form prnpos)
                      (get-doc-context :min-pos))
           endpos (if forward? (get-doc-context :max-pos) prnpos)
           seglen (- endpos begpos -1)
           mchprn (if forward? (matching-paren-of prnchr) prnchr)]
       (setup-txt-segment begpos seglen
        (find-mparen prnpos mchprn forward? begpos endpos))))
  ([prnpos mchprn forward? begpos endpos]
     (do-assert (and (<= begpos prnpos) (<= prnpos endpos)))
     (let [prnoff (- prnpos begpos)
           endoff (- endpos begpos)]
       (loop [curoff -1
              prvchr nil
              instr? false
              incmt? false
              mprlst nil ;; matching paren list [char off]
              mprstr nil ;; mprlst backup in string
              mprcmt nil] ;; mprlst backup in comment
         (let [curoff (inc curoff)]
           (if (< endoff curoff)
             [nil begpos endpos] ;; RTN
             (let [c (txtseg-char-at curoff)]
               (if (escape? prvchr)
                 ;; Ignore any escaped char.
                 (recur curoff nil instr? incmt? mprlst mprstr mprcmt)
                 (if (opening-or-closing? c)
                   ;; A paren is found.
                   (if forward?
                     ;; Stop when a matching close paren is found.
                     (if (and (< prnoff curoff)
                              (= c mchprn (ffirst mprlst))
                              (= prnoff (second (first mprlst))))
                       [(+ begpos curoff) begpos endpos] ;; RTN
                       (let [mps (if (= c (ffirst mprlst))
                                   (rest mprlst)
                                   (cons [(matching-paren-of c) curoff] mprlst))]
                         (recur curoff c instr? incmt? mps mprstr mprcmt)))
                     ;; Stop when a matching open paren is found.
                     (if (and (= curoff prnoff)
                              (= mchprn (ffirst mprlst)))
                       [(+ begpos (second (first mprlst))) begpos endpos] ;; RTN
                       (let [mps (if (= c (ffirst mprlst))
                                   (rest mprlst)
                                   (cons [(matching-paren-of c) curoff] mprlst))]
                         (recur curoff c instr? incmt? mps mprstr mprcmt))))
                   ;; Any non-paren chars
                   (cond
                    (dquote? c) (if instr?
                                  (recur curoff c false incmt? mprstr nil mprcmt)
                                  (recur curoff c true incmt? nil mprlst mprcmt))
                    (comment? c) (if (or instr? incmt?)
                                   (recur curoff c instr? incmt? mprlst mprstr mprcmt)
                                   (recur curoff c instr? true nil mprstr mprlst))
                    (newline? c) (if incmt?
                                   (recur curoff c false false mprcmt mprstr nil)
                                   (recur curoff c instr? false mprlst mprstr nil))
                    :else (recur curoff c instr? incmt? mprlst mprstr mprcmt)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-matching-paren
  "Previous closing paren has precedence unless forwoard?.
   Return [ppp fmpparam], where ppp := [open-paren-pos close-paren-pos] or nil
   and fmpparam := [forward? begpos endpos]."
  ([ced cpos]
     (find-matching-paren ced cpos nil))
  ([ced cpos dcntxt]
     (setup-doc-context ced dcntxt
      (find-matching-paren ced cpos dcntxt false)))
  ([ced cpos dcntxt forward?]
     (let [poschr (first (str-at cpos))]
       (if (or (= (get-doc-context :min-pos) cpos) forward?)
         ;; can lookup forward only.
         (if (opening? poschr)
           (let [[p1 bp ep] (find-mparen cpos poschr true)]
             (find-matching-paren ced cpos dcntxt true cpos p1 bp ep))
           [nil [true cpos cpos]]) ;; RTN
         ;; Look back if there is a closing paren.
         (if (and (< (inc (get-doc-context :min-pos)) cpos)
                  (closing-s? (str-at (dec cpos)))
                  (not (escape-s? (str-at (- cpos 2)))))
           (let [[p0 bp ep] (find-mparen (dec cpos) (first (str-at (dec cpos))) false)]
             (find-matching-paren ced cpos dcntxt false p0 (dec cpos) bp ep))
           (if (and (opening? poschr)
                    (not (escape-s? (str-at (dec cpos)))))
             (let [[p1 bp ep] (find-mparen cpos poschr true)]
               (find-matching-paren ced cpos dcntxt true cpos p1 bp ep))
             [nil [true cpos cpos]]))))) ;; RTN
  ([ced cpos dcntxt forward? p0 p1 begpos endpos]
     [(when (and p0 p1) [p0 p1]) [forward? begpos endpos]])) ;; RTN
