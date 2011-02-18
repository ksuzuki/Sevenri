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

(ns openar.refs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; core, oplix, ui

(def *openar* (ref {}))

(def *oplixes* (ref {}))
(def *oplix-on-cache* (atom (sorted-set)))
(def *opening-oplix-names* (ref #{}))

 ;; {oplix1 {key1 val1 key2 vak2 ...} oplix2 {key1 val1 key2 val2 ...} ...}
(def *xref-oplix* (ref {}))
;; {key1 {oplix1 val1 oplix2 val2} key2 {oplix3 val3 oplix4 val4 ...} ...}
(def *xref-key* (ref {}))
;; {val1 {oplix1 key1 oplix2 key2} val2 {oplix3 key3 oplix4 key4 ...} ...}
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
