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

(ns slix.ced.defs
  (:import (java.awt Color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *content-type* "text/clj")
(def *file-encoding* "UTF-8")
(def *preferred-fonts* [["Inconsolata" 'PLAIN 14] ["Courier" 'PLAIN 14]])
(def *tab-size* 8)
(def *warning-sec* 4)

(def *foreground-color* Color/black)
(def *background-color* Color/white)
(def *caret-color* Color/black)
(def *paren-highlight-color* Color/magenta)
(def *popup-bg-color* (Color. 255 255 204))

(def *prop-ced-caret-listener* "caret-listener")
(def *prop-ced-column-number* "column-number")
(def *prop-ced-find-keyword* "find-keyword")
(def *prop-ced-line-number* "line-number")
(def *prop-ced-slix* "slix")
(def *prop-ced-other-ced* "other-ced")
(def *prop-ced-ppp-info* "ppp-info") ;; ppp := paren pos pair

(def *prop-docwatcher* "docwatcher")
(def *prop-file* "file")
(def *prop-file-encoding* "fileencoding")
(def *prop-find-context* "findcontext")
(def *prop-mp-last-ced* "last-ced") ;; mp := main-panel
(def *prop-savededit* "savededit")
(def *prop-undoman* "undoman")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for indent 

(def *min-indent-prediction-lines* 300)
(def *single-comment-indent-column* 40)

(def *class-to-nargfns*
  {:defines
   {#{"defprotocol" "defstruct"} 1
    #{"defrecord" "deftype"} 2}
   :controls
   {#{"try" "do"} 0
    #{"binding" "doseq" "dotimes" "doto" "for" "handle" "handler-case"
      "if" "if-let" "if-not" "let" "letfn" "loop" "ns" "when" "when-first"
      "when-let" "when-not" "while" "with-open" "with-local-vars"} 1
    #{"catch" "condp"} 2}
   :core-fns
   {#{"invoke" "reify"} -1
    #{"comment" "future"} 0
    #{"assoc" "extend" "extend-protocol" "extend-type" "locking" "struct-map"
      "with-open" "with-precision"} 1
    #{"proxy"} 2}
   :core-lib-fns
   {#{"deftest" "testing"} 1}})

(def *max-backtracking* 3)

(def *backtrack-indent-fns*
  ["defprotocol" "defrecord" "deftype"
   "extend-protocol" "extend-type"
   "letfn" "proxy" "reify"])
