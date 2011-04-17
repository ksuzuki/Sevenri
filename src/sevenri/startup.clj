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

(ns ^{:doc "Sevenri startup and shutdown library"}
  sevenri.startup
  (:use [sevenri config defs log]
        [sevenri.utils :only (elapsed-msecs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn resolve-using-fns
  []
  (print-info "Start resolving using fns.")
  (let [start (System/nanoTime)]
    (doseq [fquns (filter #(re-matches #"^sevenri\..*" (str (ns-name %))) (all-ns))]
      (let [rep (re-pattern (str "^" (using-fn-prefix fquns) ".*"))]
        (doseq [ufn (filter #(re-matches rep (str %)) (keys (ns-interns fquns)))]
          (let [ufnv (ns-resolve fquns ufn)]
            (if (and ufnv (fn? (var-get ufnv)))
              (let [[rfn rns] (ufnv)
                    fqrns (symbol (str 'sevenri \. rns))]
                (print-info (format "%s/%s -> %s/%s" (ns-name fquns) ufn fqrns rfn))
                (intern fquns ufn (ns-resolve fqrns rfn)))
              (print-severe "Failed to resolve" (format "%s/%s" (ns-name fquns) ufn)))))))
    (print-info "End resolving using fns:" (elapsed-msecs start (System/nanoTime)) "msecs")))

(defn startup-or-shutdown
  [kwd]
  (let [startup (= kwd :startup)
        odr (if startup *startup-order* (reverse *startup-order*))
        pfx (if startup 'startup 'shutdown)]
    (print-info (if startup "Startup" "Shutdown") "process started.")
    (doseq [i odr]
      (let [n (symbol (str 'sevenri \. i))
            s (symbol (str pfx \- i \?))
            v (ns-resolve n s)]
        (print-info "[" (:name (meta v)) "]")
        (if (and v (fn? (var-get v)))
          (let [stime (System/nanoTime)
                rtval (v)
                etime (System/nanoTime)]
            (print-info "Elapsed time:" (elapsed-msecs stime etime) "msecs")
            (when-not rtval
              (let [m (print-str "startup-or-shutdown:" pfx "failed on" v)]
                (if startup
                  (throw (RuntimeException. m))
                  (log-severe m)))))
          (let [m (print-str "startup-or-shutdown: not found" s)]
            (if startup
              (throw (NoSuchMethodException. m))
              (lg m))))))
    (print-info (if startup "Startup" "Shutdown") "process completed.")
    ;;
    (when startup
      (resolve-using-fns))))
