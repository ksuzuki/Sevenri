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

(ns slix.ced.indent
  (:use [sevenri log]
        [slix.ced defs fnclasses refs]
        [library.slix.ced doclib paren])
  (:import (java.util.regex Pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-unbalancing-opening-context
  "Return [ubo-pos uo-ctxt], where
   ubo-pos := nil when aborted, -1 when not unbalancing, or a number>=0
   uo-ctxt := [begpos endpos curoff prvchr instr? oprlst]"
  ([pos check-min-lines?]
     (let [begpos (or (top-level-form pos) (get-doc-context :min-pos))
           abort? (and check-min-lines?
                       (< (- (line-index pos) (line-index begpos)) *min-indent-prediction-lines*))]
       (if abort?
         [nil [begpos pos 0 nil false nil]] ;; RTN
         (get-unbalancing-opening-context begpos pos 0 nil false nil))))
  ([beg-pos end-pos cur-off prv-chr in-str? opr-lst]
     (let [endoff (- end-pos beg-pos)
           edlidx (line-index end-pos)]
     (setup-txt-segment beg-pos endoff
       (loop [curoff (dec cur-off)
              prvchr prv-chr
              instr? in-str?
              oprlst opr-lst] ;; [chr off]
         (let [curoff (inc curoff)]
           (if (<= endoff curoff)
             ;; Reached at the end.
             (let [uotctxt [beg-pos end-pos curoff prvchr instr? oprlst]]
               (if (and (not instr?) (seq oprlst))
                 ;; An unbalancing opening paren exists.
                 [(+ beg-pos (second (first oprlst))) uotctxt] ;; RTN
                 ;; No unbalancing opening paren exists.
                 [-1 uotctxt])) ;; RTN
             ;; Continue scanning.
             (let [c (txtseg-char-at curoff)]
               (if (escape? prvchr)
                 ;; Ignore any escaped char.
                 (recur curoff nil instr? oprlst)
                 (if instr?
                   ;; Continue in string, or get out of it.
                   (recur curoff c (if (dquote? c) false true) oprlst)
                   (if (opening-or-closing? c)
                     ;; A paren is found.
                     (recur curoff c false (if (= (matching-paren-of c) (ffirst oprlst))
                                             (rest oprlst)
                                             (cons [c curoff] oprlst)))
                     ;; Any non-paren chars.
                     (cond
                      (dquote? c) (recur curoff c true oprlst)
                      (comment? c) (let [clidx (line-index (+ beg-pos curoff))]
                                     (recur (dec (if (<= edlidx clidx)
                                                   endoff
                                                   (- (line-index-to-end-pos clidx) beg-pos)))
                                            c false oprlst))
                      :else (recur curoff c instr? oprlst)))))))))))))

(defn find-unbalancing-opening
  "[ubo-pos ubo-column ubo-txtseg uo-ctxt]
   ubo-pos/ubo-column := nil when aborted, -1 when not unbalancing, or a number>=0
   ubo-txtseg := nil when aborted or not unbalancing, or a segment object"
  ([pos check-min-lines?]
     (find-unbalancing-opening (get-unbalancing-opening-context pos check-min-lines?)))
  ([begpos endpos curoff prvchr instr? oprlst]
     (find-unbalancing-opening (get-unbalancing-opening-context begpos endpos curoff prvchr instr? oprlst)))
  ([pos-ctxt]
     (let [[ubo-pos uo-ctxt] pos-ctxt]
       (if ubo-pos
         (if (neg? ubo-pos)
           ;; Unbalancing opening not found.
           [-1 -1 nil uo-ctxt]
           ;; Unbalancing opening found.
           (let [end-pos (second uo-ctxt)]
             [ubo-pos (pos-to-col ubo-pos) (get-txtseg ubo-pos (- end-pos ubo-pos)) uo-ctxt]))
         ;; Finding unbalancing opening aborted.
         [nil nil nil uo-ctxt]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn indent-column-element-indecies
  "Return a vector of out-most element's indecies."
  [ubo-ctxt]
  (let [[beg _ txtseg _] ubo-ctxt
        len (.length txtseg)]
    (setup-txt-segment beg len
     (loop [idx 1
            ids []]
       (if (<= len idx)
         ids
         (let [c (txtseg-char-at idx)]
           (cond
            (closing? c) ids
            (space? c) (recur (inc idx) ids)
            (comment? c) (recur (- (end-of-line (+ beg idx)) beg) ids)
            (opening? c) (let [[opcl-pos fmpparan] (find-matching-paren
                                                    (get-doc-context :ced)
                                                    (+ beg idx)
                                                    nil
                                                    true)]
                           (do-assert (vector? opcl-pos))
                           (recur (- (second opcl-pos) beg -1) (conj ids idx)))
            :else (recur (loop [i idx
                                ins? false]
                           (if (<= len i)
                             i
                             (let [c (txtseg-char-at i)]
                               (if (and (not ins?)
                                        (or (closing? c) (space? c) (comment? c) (opening? c)))
                                 i
                                 (if (escape? c)
                                   (recur (+ i 2) ins?)
                                   (recur (inc i) (if (dquote? c)
                                                    (if ins? false true)
                                                    ins?)))))))
                         (conj ids idx)))))))))

(defn indent-column-func-backtrack?
  [ubo-ctxt]
  (let [[begpos endpos _ _ _ oprlst] (last ubo-ctxt)
        seglen (- endpos begpos)]
    (setup-txt-segment begpos seglen
      (loop [bktcnt 0
             oprlst oprlst]
        (if (or (not (seq oprlst)) (<= *max-backtracking* bktcnt))
          false
          (let [[_ off] (first oprlst)
                ssq (.subSequence *txtseg* off seglen)
                rem (.reset (re-matcher #"^[({\[]\s*([^\s;]+)\s*" "") ssq)
                rff (re-find rem)
                fqs (if rff (second rff) "")
                rfs (re-find #"(.*/)?([^/]+)" fqs)
                sym (if rfs (last rfs) "")]
            #_(lg "ssq:" ssq "sym:" sym)
            (if (or (opening-s? sym)
                    (not (some #(= sym %) *backtrack-indent-fns*)))
              (recur (inc bktcnt) (rest oprlst))
              true)))))))

(defn indent-column-func-default
  "+1 with no arg by default. Align to the 1st arg pos otherwise.
   EXCEPTION: +2 in the context of the 'defn' siblings, 'with-' macro, and
   backtracking."
  ([ubo-ctxt fqsym]
     (indent-column-func-default ubo-ctxt fqsym 1))
  ([ubo-ctxt fqsym adj]
     #_(lg "ubo-ctxt:" ubo-ctxt "fqsym:" fqsym "adj:" adj)
     (if (or (some #(re-find % fqsym) [#"^def" #"^with-"])
             (indent-column-func-backtrack? ubo-ctxt))
       2
       (let [ubo-pos (first ubo-ctxt)
             ubo-eis (indent-column-element-indecies ubo-ctxt)]
         (if (< (count ubo-eis) 2)
           (let [ubo-lnidx (line-index ubo-pos)
                 ubo-lntxt (str-at ubo-pos (- (line-index-to-end-pos ubo-lnidx) ubo-pos))]
             (+ (.indexOf ubo-lntxt fqsym) -1 adj))
           (- (pos-to-col (+ ubo-pos (second ubo-eis))) (pos-to-col ubo-pos)))))))

(defn indent-column-func
  "n args:      +4 for args. +2 for the rest.
   0 arg:       +2 with no no arg. Align to the 1st arg pos otherwise.
   -1 arg:      +2. Do the same as defines.
   defines:     +2 always. Do the same for lambda-expr too.
   default:     +1 with no arg by default. Align to the 1st arg pos otherwise."
  ([ubo-ctxt opc fqsym sym cls]
     #_(lg "ubo-ctxt:" ubo-ctxt "fqsym:" fqsym "cls:" cls)
     (loop [nargfns (cls *class-to-nargfns*)]
       (if (seq nargfns)
         (let [[fnset narg] (first nargfns)]
           (if (get fnset sym)
             (indent-column-func ubo-ctxt fqsym narg)
             (recur (rest nargfns))))
         (if (or (= cls :defines) (= cls :lambda-expr))
           2
           (indent-column-func-default ubo-ctxt fqsym 1)))))
  ([ubo-ctxt fqsym narg]
     #_(lg "ubo-ctxt:" ubo-ctxt "fqsym:" fqsym "narg:" narg)
     (if (pos? narg)
       ;; +4 for args. +2 for the rest.
       (let [eis (indent-column-element-indecies ubo-ctxt)
             nis (count eis)]
         (cond
          (and (= narg 2) (< nis 3)) 4
          (and (= narg 1) (< nis 2)) 4
          :else 2))
       (if (zero? narg)
         ;; +2 with no arg. Align to the 1st arg pos otherwise.
         (indent-column-func-default ubo-ctxt fqsym 2)
         ;; +2. Do the same as defines.
         2))))

(defn indent-column-expr
  [ubo-ctxt opc fqsym sym]
  (let [[ubo-pos _ txtseg _] ubo-ctxt
        ts-subseq (.subSequence txtseg 0 (.length txtseg))
        fqsym-rem (.reset (re-matcher (Pattern/compile fqsym Pattern/LITERAL) "") ts-subseq)
        fqsym-col (if (.find fqsym-rem)
                    (pos-to-col (+ ubo-pos (.start (.toMatchResult fqsym-rem))))
                    (do-assert false)) ;; Finding fqsym must suceed.
        fqsym-off (- fqsym-col (pos-to-col ubo-pos))
        fqsym-chr (.charAt txtseg fqsym-off)
        argmt-off (when-let [rfa (re-find #"^([(\[{]+)\s*((['@#^~])?[^\s;]+)" fqsym)]
                    (let [[g1 g2 g3] rfa]
                      (- (count g1) (count g2) (count g3))))]
    #_(lg "txtseg:" txtseg "fqsym:" fqsym "fqsym-off" fqsym-off "argmt-off:" argmt-off)
    (if (= opc \()
      (if (= fqsym-chr \[)
        3 ;; most likely '([params]
        (if argmt-off
          (+ fqsym-off argmt-off) ;; other openings
          (indent-column-func-default ubo-ctxt fqsym))) ;; user defined fn
      (+ fqsym-off (or argmt-off 0)))))

(defn indent-column-delta
  [ubo-ctxt]
  (let [[ubo-pos _ txtseg _] ubo-ctxt
        opc (.charAt txtseg 0)
        ssq (.subSequence txtseg 0 (.length txtseg))
        rem (.reset (re-matcher #"^[({\[]\s*([^\s;]+)\s*" "") ssq)
        rff (re-find rem)
        fqs (if rff (second rff) "")
        rfs (re-find #"(.*/)?([^/]+)" fqs)
        sym (if rfs (last rfs) "")
        cls (when-not (empty? sym)
              (loop [fncls *fn-classes*]
                (when (seq fncls)
                  (if (get (second (first fncls)) sym)
                    (ffirst fncls)
                    (recur (rest fncls))))))]
    #_(lg "opc:" opc "ssq:" ssq "fqs:" fqs "cls:" cls)
    (if (empty? fqs)
      1
      (if (and (= opc \() cls)
        (indent-column-func ubo-ctxt opc fqs sym cls)
        (indent-column-expr ubo-ctxt opc fqs sym)))))

(defn calc-indent-column
  "Return a column number or nil."
  [ubo-ctxt]
  (let [[_ ubo-column _ _] ubo-ctxt]
    (when ubo-column
      (if (neg? ubo-column)
        ubo-column
        (+ ubo-column (indent-column-delta ubo-ctxt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-new-indent-prediction
  "Return the old indent prediction consisted of a prediction position and
   the future indent object for the position."
  [indent-prediction]
  (dosync
   (let [[pred-pos ind-pred doc-size] @*indent-prediction*]
     (when (and indent-prediction (not (future-done? ind-pred)))
       (future-cancel ind-pred)
       #_(lg "old indent prediction cancelled"))
     (ref-set *indent-prediction* (or indent-prediction [nil (future nil) nil]))
     [pred-pos ind-pred doc-size])))

(defn predict-indent
  "Return a new indent prediction. By default check-min-lines? is true, unless
   a fresh prediction is required, and the old prediction context is reused
   when possible, that is, the current pos is the right next, the old
   prediction has been completed, and the doc size is the same."
  ([ced]
     (predict-indent ced true))
  ([ced check-min-lines?]
     (let [pred-pos (.getCaretPosition ced)
           doc-size (.getLength (.getDocument ced))
           old-ctxt (when check-min-lines?
                      (let [[old-ppos old-iprd old-size] @*indent-prediction*]
                        (when (and (= old-ppos (dec pred-pos))
                                   (future-done? old-iprd)
                                   (= (last @old-iprd) doc-size))
                          #_(lg "Reusing old-ctxt")
                          (second @old-iprd))))
           ind-pred (vector
                     pred-pos
                     (future
                       (setup-doc-context ced nil
                         (let [doc-size (.getLength (.getDocument ced))
                               ubo-contxt (if old-ctxt
                                            (let [[begpos endpos curoff prvchr instr? oprlst] (last old-ctxt)]
                                              (find-unbalancing-opening begpos pred-pos curoff prvchr instr? oprlst))
                                            (find-unbalancing-opening pred-pos check-min-lines?))
                               indent-col (calc-indent-column ubo-contxt)]
                           ;;
                           #_(let [lidx (line-index (get-doc-context :pos))
                                   clfc (first-char (line-index-to-start-pos lidx))
                                   nlfc (when (< lidx (dec (get-doc-context :doc-ect)))
                                          (first-char (line-index-to-start-pos (inc lidx))))
                                   clcl (when clfc (pos-to-col (first clfc)))
                                   nlcl (when nlfc (pos-to-col (first nlfc)))]
                               (lg "check-min-lines?:" check-min-lines?
                                   "pred-pos:" pred-pos
                                   "ubo-context:" ubo-contxt
                                   "\npredicted:" indent-col "current:" clcl "next:" nlcl))
                           ;;
                           [indent-col ubo-contxt doc-size]))))]
       (set-new-indent-prediction ind-pred)
       ind-pred)))

(defn get-indent-column
  ([ced]
     (get-indent-column ced true))
  ([ced use-prediction?]
     (let [[pred-pos ind-pred doc-size] (set-new-indent-prediction nil)
           crt-pos (.getCaretPosition ced)
           pred-col (when (and use-prediction? (= crt-pos pred-pos) ind-pred)
                      (let [pc (first @ind-pred)]
                        #_(lg "get-indent-column: using a predicted column:" pc)
                        pc))]
       (or pred-col (first @(second (predict-indent ced false)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-line
  [beg txt]
  (let [[start len] (start-and-len-line beg)
        doc (get-doc-context :doc)]
    (do-assert (= beg start))
    (.remove doc start (dec len)) ;; leave \n
    (.insertString doc
                   start
                   (if (newline? (last txt))
                     (subs txt 0 (count txt))
                     txt)
                   nil)))

(defn indent-line
  ([col beg txt fc-pos lc-pos]
     (indent-line col beg fc-pos (subs txt (- fc-pos beg) (inc (- lc-pos beg)))))
  ([col beg fc-pos txt]
     (let [fc-col (- fc-pos beg)]
       (when (not= col fc-col)
         (replace-line beg (str (apply str (repeat col \space)) txt))))))

(defn indent-blank-line
  [col beg len]
  (indent-line col beg (+ beg (dec len)) ""))

(defn indent-comment-line
  [col beg txt fc-pos lc-pos]
  (if (re-find #"^\s*;([^;]+.*)?\n$" txt)
    ;; a ";" line
    (when-not (= (- fc-pos beg) *single-comment-indent-column*)
      (replace-line beg
                    (str (apply str (repeat *single-comment-indent-column* \space))
                         (subs txt (- fc-pos beg) (inc (- lc-pos beg))))))
    (indent-line col beg fc-pos (subs txt (- fc-pos beg) (inc (- lc-pos beg))))))

(defn indent-by-column
  [ced column]
  (let [pos (.getCaretPosition ced)
        [sol-pos len] (start-and-len-line pos)]
    ;;
    (if-let [[lc-pos beg seg fc-pos] (last-char pos)]
      (if (comment? (.charAt seg (- fc-pos beg)))
        (indent-comment-line column beg (.toString seg) fc-pos lc-pos)
        (indent-line column beg (.toString seg) fc-pos lc-pos))
      (indent-blank-line column sol-pos len))
    ;;
    (if-let [fc (first-char sol-pos)]
      (first fc)
      (end-of-line sol-pos))))

(defn indent
  [ced column]
  (when (and (number? column) (pos? column))
    (indent-by-column ced column)))

(defn indent-region
  [ced pos prv-contxt]
  (let [contxt (if prv-contxt
                 (let [[begpos endpos curoff prvchr instr? oprlst] (last prv-contxt)]
                   (find-unbalancing-opening begpos pos curoff prvchr instr? oprlst))
                  (find-unbalancing-opening pos false))
        column (calc-indent-column contxt)]
    (when (and (number? column) (pos? column))
      (indent-by-column ced column))
    (when (seq (last (last contxt)))
      contxt)))
