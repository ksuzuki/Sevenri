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

(ns Sevenri
  (:gen-class)
  (:require [sevenri defs refs]
            [sevenri config core event jvm log os props slix ui]
            [sevenri debug main startup utils]))

(defn -main
  [& args]
  (sevenri.main/run args))
