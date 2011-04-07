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

(ns slix.repl.defs
  (:import (java.awt Color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *console-background-color* Color/lightGray)
(def *console-prompt-color* Color/yellow)
(def *ppp-info* "ppp-info")
(def *text-caret-color* Color/yellow)
(def *text-background-color* (Color. 50 50 100))
(def *text-foreground-color* Color/white)
(def *opacity-default* 0.90)
(def *opacity-delta* 0.02)
(def *prop-opacity* "opacity")
