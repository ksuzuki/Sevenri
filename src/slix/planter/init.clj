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
        frm (slix-frame)
        ao? (alt-open-slix?)]
    (future
      (if (is-project-built? 'slix.planter)
        (planter-project-ready true)
        (let [oc (.getCursor frm)]
          (.setCursor frm Cursor/WAIT_CURSOR)
          ;;
          (planter-project-ready false)
          (when ao?
            (let [bp? (build-project? 'slix.planter)
                  msg (if bp? "succeeded" "failed")]
              (when bp?
                (planter-project-ready true))
              (lg "slix.planter: verify: building the planter project" msg)))
          (let [b? (is-project-built? 'slix.planter)]
            (when-not b?
              (log-warning "slix.planter: verify: planter project isn't ready.")
              (Thread/sleep (* 1000 3)))
            ;;
            (.setCursor frm oc)
            (when-not b?
              (close-slix slx))))))))
