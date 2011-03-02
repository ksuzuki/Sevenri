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

(ns library.slix.ced.doclib
  (:use [sevenri log])
  (:import (javax.swing.text Segment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *dcntxt* nil)
(def ^:dynamic *txtseg* nil)

(def *open-close-pairs* {\( [\( \)]
                         \{ [\{ \}]
                         \[ [\[ \]]
                         \) [\) \(]
                         \} [\} \{]
                         \] [\] \[]})

(def *open-close-pairs-s* {"(" ["(" ")"]
                           "{" ["{" "}"]
                           "[" ["[" "]"]
                           ")" [")" "("]
                           "}" ["}" "{"]
                           "]" ["]" "["]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-doc-context
  [ced]
  (let [doc (.getDocument ced)
        doc-elm (.getDefaultRootElement doc)]
    {:ced ced
     :doc doc
     :pos (.getCaretPosition ced)
     :min-pos 0
     :max-pos (dec (.getLength doc))
     :doc-elm doc-elm
     :doc-ect (.getElementCount doc-elm)}))

(defmacro setup-doc-context
  [ced dcntxt & body]
  `(binding [~'*dcntxt* (merge (create-doc-context ~ced) ~dcntxt)]
     ~@body))

(defmacro get-doc-context
  ([]
     `~'*dcntxt*)
  ([kwd]
     `(~kwd ~'*dcntxt*)))

(defmacro update-doc-context
  [dcntxt & body]
  `(binding [~'*dcntxt* (merge (get-doc-context) ~dcntxt)]
     ~@body))

(defmacro do-assert
  [x]
  ;;`(#_(assert ~x)))
  `(assert ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro partial-segment
  ([]
     `(.setPartialReturn ~'*txtseg* true))
  ([seg]
     `(.setPartialReturn ~seg true)))

(defmacro get-partial-txtseg
  [beg len]
  `(let [seg# (Segment.)]
     (partial-segment seg#)
     (.getText (get-doc-context :doc) ~beg ~len seg#)
     seg#))

(defmacro exact-segment
  ([]
     `(.setPartialReturn ~'*txtseg* false))
  ([seg]
     `(.setPartialReturn ~seg false)))

(defmacro get-exact-txtseg
  [beg len]
  `(let [seg# (Segment.)]
     (exact-segment seg#)
     (.getText (get-doc-context :doc) ~beg ~len seg#)
     seg#))

(defmacro get-txtseg-begin-end
  ([]
     `(let [seg# ~'*txtseg*]
       [(.getBeginIndex seg#) (.getEndIndex seg#)]))
  ([seg]
     `[(.getBeginIndex ~seg) (.getEndIndex ~seg)]))

(defmacro get-txtseg-begin-len
  ([]
     `(let [seg# ~'*txtseg*]
        [(.getBeginIndex seg#) (.length seg#)]))
  ([seg]
     `[(.getBeginIndex ~seg) (.length ~seg)]))

(defn get-txtseg
  [beg len]
  (let [seg (get-partial-txtseg beg len)
        [_ slen] (get-txtseg-begin-len seg)]
    (if (= len slen)
      seg
      (let [seg (get-exact-txtseg beg len)]
        (do-assert (= len (.length seg)))
        #_(lg "exact seg for beg:" beg "len:" len)
        seg))))

(defmacro setup-txt-segment
  [beg len & body]
  `(binding [~'*txtseg* (get-txtseg ~beg ~len)]
     ~@body))

(defmacro txtseg-char-at
  ([off]
     `(.charAt ~'*txtseg* ~off))
  ([seg off]
     `(.carAt ~seg ~off)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro str-at
  ([pos]
     `(.getText (get-doc-context :doc) ~pos 1))
  ([pos len]
     `(.getText (get-doc-context :doc) ~pos ~len)))

(defmacro line-index
  [pos]
  `(.getElementIndex (get-doc-context :doc-elm) ~pos))

(defmacro line-index-to-start-pos
  [lindex]
  `(.getStartOffset (.getElement (get-doc-context :doc-elm) ~lindex)))

(defmacro line-index-to-end-pos
  [lindex]
  `(.getEndOffset (.getElement (get-doc-context :doc-elm) ~lindex)))

(defmacro start-of-line
  [pos]
  `(.getStartOffset (.getParagraphElement (get-doc-context :doc) ~pos)))
  
(defmacro end-of-line
  [pos]
  `(dec (.getEndOffset (.getParagraphElement (get-doc-context :doc) ~pos))))

(defmacro start-and-end-line
  [pos]
  `(let [e# (.getParagraphElement (get-doc-context :doc) ~pos)]
     [(.getStartOffset e#) (.getEndOffset e#)]))

(defmacro start-and-len-line
  [pos]
  `(let [e# (.getParagraphElement (get-doc-context :doc) ~pos)
         s# (.getStartOffset e#)]
     [s# (- (.getEndOffset e#) s#)]))

(defmacro pos-to-col
  [pos]
  `(- ~pos (start-of-line ~pos)))

(defn start-of-prev-line
  [pos]
  (let [doc-elm (get-doc-context :doc-elm)
        idx (.getElementIndex doc-elm pos)]
    (when (< (get-doc-context :min-pos) idx)
      (.getStartOffset (.getElement doc-elm (dec idx))))))

(defn end-of-prev-line
  [pos]
  (let [doc-elm (get-doc-context :doc-elm)
        idx (.getElementIndex doc-elm pos)]
    (when (< (get-doc-context :min-pos) idx)
      (dec (.getEndOffset (.getElement doc-elm (dec idx)))))))

(defmacro newline?
  [c]
  `(= ~c \newline))

(defmacro newline-s?
  [s]
  `(= ~s "\n"))

(defmacro space?
  [c]
  `(or (= ~c \space) (= ~c \tab) (= ~c \newline)))

(defmacro space-s?
  [s]
  `(or (= ~s " ") (= ~s "\t") (= ~s "\n")))

(defmacro escape?
  [c]
  `(= ~c \\))

(defmacro escape-s?
  [s]
  `(= ~s "\\"))

(defmacro comment?
  [c]
  `(= ~c \;))

(defmacro comment-s?
  [s]
  `(= ~s ";"))

(defmacro dquote?
  [c]
  `(= ~c \"))

(defmacro dquote-s?
  [s]
  `(= ~s "\""))

(defmacro opening?
  [c]
  `(or (= ~c \() (= ~c \{) (= ~c \[)))

(defmacro opening-s?
  [s]
  `(or (= ~s "(") (= ~s "{") (= ~s "[")))

(defmacro closing?
  [c]
  `(or (= ~c \]) (= ~c \}) (= ~c \))))

(defmacro closing-s?
  [s]
  `(or (= ~s "]") (= ~s "}") (= ~s ")")))

(defmacro opening-or-closing?
  [c]
  `(get *open-close-pairs* ~c))

(defmacro opening-or-closing-s?
  [s]
  `(get *open-close-pairs-s* ~s))

(defmacro matching-paren-of
  [c]
  `(second (get *open-close-pairs* ~c)))

(defmacro matching-paren-of-s
  [s]
  `(second (get *open-close-pairs-s* ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn first-char
  "Returns [pos beg seg] or nil"
  ([pos]
     (let [[s l] (start-and-len-line pos)]
       (first-char (get-txtseg s l) s 0 l)))
  ([seg beg off len]
     (when (< off len)
       (if (space? (.charAt seg off))
         (recur seg beg (inc off) len)
         [(+ beg off) beg seg]))))

(defn last-char
  "Returns [pos beg seg first] or nil"
  [pos]
  (when-let [a (first-char pos)]
    (let [[first beg seg] a
          len (.length seg)]
      (loop [lof (- first beg)
             off lof]
        (if (= off len)
          [(+ beg lof) beg seg first]
          (recur (if (space? (.charAt seg off)) lof off) (inc off)))))))

(defn last-code-char
  "Returns [pos beg seg first] or nil"
  [pos]
  (when-let [a (first-char pos)]
    (let [[first beg seg] a
          len (.length seg)]
      (loop [lof nil
             off (- first beg)
             lc 0
             ins? false]
        (if (= off len)
          (when lof
            [(+ beg lof) beg seg first])
          (let [c (.charAt seg off)]
            (if ins?
              (if (dquote? c)
                (recur off (inc off) c (if (escape? lc) true false))
                (recur off (inc off) c true))
              (if (and (comment? c)
                       (or (zero? off)
                           (and (pos? off) (not (escape? (.charAt seg (dec off)))))))
                (when lof
                  [(+ beg lof) beg seg first])
                (if (dquote? c)
                  (recur off (inc off) c (if (escape? lc) false true))
                  (recur (if (space? c) lof off) (inc off) c false))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn top-level-form
  "Return pos or nil. All top level forms should start at the first column
   after a blank line, except at the beginning of the document."
  [pos]
  (letfn [(abl [p]
            (if (= p (get-doc-context :min-pos))
              p
              (when-not (last-code-char (dec p))
                p)))
          (tlf [p l]
            (let [s (str-at p)]
              (if (= s "(")
                (abl p)
                (when (and (= s "#")
                           (re-matches #"^(#_\s*)?\(.*\n$" (str-at p l)))
                    (abl p)))))]
    (if-let [p (apply tlf (start-and-len-line pos))]
      p
      (loop [pos (start-of-prev-line pos)]
        (when pos
          (if-let [p (apply tlf (start-and-len-line pos))]
            p
            (recur (start-of-prev-line pos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-symbol
  ([ced]
     (setup-doc-context ced nil
      (let [pos (get-doc-context :pos)
            [s l] (start-and-len-line pos)
            off (- pos s)]
        (setup-txt-segment s l
          (if (space? (txtseg-char-at off))
            (when (pos? off)
              (when-not (space? (txtseg-char-at (dec off)))
                (fetch-symbol ced s l (dec off))))
            (fetch-symbol ced s l off))))))
  ([ced start len off]
     (loop [off off]
       (if (and (pos? off)
                (not (space? (txtseg-char-at off))))
         (recur (dec off))
         (when-let [match (re-matches #"[^\w\*\+\!\-\_\?\.]*([\w\*\+\!\-\_\?\.\$]+).*\n$"
                                      (.toString (.subSequence *txtseg* off len)))]
           #_(lg "fetched symbol:" (second match))
           (second match))))))
