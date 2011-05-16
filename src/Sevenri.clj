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

(ns ^{:doc "Sevenri bootloader"}
  Sevenri
  (:gen-class)
  (:require [sevenri config core log props slix ui]
            [sevenri event java jvm os]
            [sevenri debug main startup utils]
            [sevenri defs refs]))

(defn -main
  [& args]
  (sevenri.main/run args))
