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

(ns ^{:doc "Sevenri refs"}
  sevenri.refs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; core, slix, ui

(def *sevenri* (ref {}))

(def *slixes* (ref {}))
(def *slix-sn-cache* (atom (sorted-set)))
(def *opening-slix-names* (ref #{}))

 ;; {slix1 {key1 val1 key2 vak2 ...} slix2 {key1 val1 key2 val2 ...} ...}
(def *xref-slix* (ref {}))
;; {key1 {slix1 val1 slix2 val2} key2 {slix3 val3 slix4 val4 ...} ...}
(def *xref-key* (ref {}))
;; {val1 {slix1 key1 slix2 key2} val2 {slix3 key3 slix4 key4 ...} ...}
(def *xref-val* (ref {}))

(def *z-order* (atom [{} nil]))

;; temporary storages
(def *frames-snapshot* (atom nil))
(def *frame-popup* (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; event

(def *last-event* (atom nil))
(def *last-error-event* (atom nil))
(def *last-global-event* (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; log

(def *e* (atom nil))
(def *exception-listeners-cache* (atom {}))
