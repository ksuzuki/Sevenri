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

(ns slix.sevenri.aotlist
  (:use [sevenri log slix os]
        [slix.sevenri.ui :only (enable-main-panel)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *aot-compile-list* '[ced])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn aot-compile-slixes
  []
  (let [frame (slix-frame)]
    (future
      (let [msg "sevenri: aot-compile-slixes: aot-compile failed for:"]
        (enable-main-panel frame false)
        ;;
        (doseq [sn *aot-compile-list*]
          (if (aot-compile? sn 'aot)
            (when-let [aot-os (cond
                               (is-mac?) 'aot-mac
                               :else nil)]
              (when (.exists (get-slix-path sn (str aot-os '!clj)))
                (when-not (aot-compile? sn aot-os)
                  (log-warning msg aot-os))))
            (log-warning msg sn)))
        ;; 
        (enable-main-panel frame true)))))
