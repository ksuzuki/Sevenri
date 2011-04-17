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

(ns ^{:doc "Sevenri debug library"}
  sevenri.debug
  (:use [sevenri config defs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-swank-repl-running?
  []
  *swank-repl-is-running*)

(defn run-swank-repl
  ([]
     (when (System/getenv (get-config 'debug.env-name))
       (run-swank-repl true)))
  ([run]
     (when (and run (not (is-swank-repl-running?)))
       (require 'swank.swank)
       ((ns-resolve 'swank.swank 'start-repl)
        (get-config 'debug.swank-port)
        :encoding (get-config 'debug.swank-encoding))
       (reset-swank-repl-state true))))
