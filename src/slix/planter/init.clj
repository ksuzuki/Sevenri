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

(ns slix.planter.init
  (:use [sevenri log slix]
        [slix.planter core defs])
  (:import (java.awt Cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn verify
  []
  (let [slx *slix*
        frm (slix-frame slx)
        ao? (alt-open-slix? slx)]
    (future
      (if (is-project-built? 'slix.planter)
        (planter-project-ready true)
        (let [oc (.getCursor frm)]
          (.setCursor frm Cursor/WAIT_CURSOR)
          ;;
          (if ao?
            (let [bp? (build-project? 'slix.planter)]
              (lg "slix.planter: self-diag: building the planter project" (if bp?
                                                                            "succeeded"
                                                                            "failed")))
            (do
              (log-warning "slix.planter: self-diag: planter project isn't ready yet")
              (Thread/sleep (* 1000 3))))
          ;;
          (.setCursor frm oc)
          (close-slix slx))))))
