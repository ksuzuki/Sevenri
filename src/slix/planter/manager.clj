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

(ns slix.planter.manager
  (:use [slix.planter core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project-manager
  []
  (reify
    sevenri.core.PProjectManager
    (get-name [_]
      'slix.planter)
    (ready? [_]
      (is-project-built? 'slix.planter))
    (setup? [_]
      (build-project*? 'slix.planter))
    (shutdown [_]
      (shutdown-manager))
    ;;
    (exists? [_ projname]
      (project-exists? projname))
    (built? [_ projname]
      (is-project-built? projname))
    (build [_ projname]
      (build-project projname))
    (get-jars [_ projname]
      (get-project-all-jars projname))
    (build-and-run [_ projname slix-name name args]
      (build-project-and-run projname slix-name name args))))
