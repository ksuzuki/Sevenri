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

(ns sevenri.startup
  (:use [sevenri config defs log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resolve-using-fns
  []
  (let [ops (get-default :tln :sevenri)
        rep (re-pattern (str "^" ops "\\..*"))]
    (doseq [fquns (filter #(re-matches rep (str (ns-name %))) (all-ns))]
      (let [rep (re-pattern (str "^" (using-fn-prefix fquns) ".*"))]
        (doseq [ufn (filter #(re-matches rep (str %)) (keys (ns-interns fquns)))]
          (let [ufnv (ns-resolve fquns ufn)]
            (when (and ufnv (fn? (var-get ufnv)))
              (let [[rfn rns] (ufnv)
                    fqrns (symbol (str ops \. rns))]
                (intern fquns ufn (ns-resolve fqrns rfn))))))))))

(defn startup-or-shutdown
  [kwd]
  (let [startup (= kwd :startup)
        odr (if startup *startup-order* (reverse *startup-order*))
        pfx (if startup 'startup 'shutdown)]
    (doseq [i odr]
      (let [o (get-default :tln :sevenri)
            n (symbol (str o \. i))
            s (symbol (str pfx \- i \?))
            v (ns-resolve n s)]
        (print-info (if startup "starting up:" "shutting down:") "[" (:name (meta v)) "]")
        (if (and v (fn? (var-get v)))
          (when-not (v)
            (let [m (print-str "startup-or-shutdown:" pfx "failed on" v)]
              (if startup
                (throw (RuntimeException. m))
                (log-severe m))))
          (let [m (print-str "startup-or-shutdown: not found" s)]
            (if startup
              (throw (NoSuchMethodException. m))
              (lg m))))))
    ;;
    (when startup
      (resolve-using-fns))))
