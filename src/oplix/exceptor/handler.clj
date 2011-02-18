;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Openar, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns oplix.exceptor.handler
  (:use [openar log]
        [oplix.exceptor core defs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-on-compiler-exception
  [#^Exception e fqon]
  (let [cmsg (.getMessage e)
        mtch (re-matches #".*\(([^:]+):(\d+)\)$" cmsg)]
    #_(lg "fqon:" fqon "cmsg:" cmsg "mtch:" mtch "(rest mtch):" (rest mtch))
    (when mtch
      (let [[fname line] (rest mtch)]
        (launch-exceptor e fqon fname line)))))

(defn find-on-ste
  [#^Exception e]
  (let [fos (reduce (fn [foa ste]
                      (let [cls (.getClassName ste)
                            mtd (.getMethodName ste)
                            rem (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" cls)]
                        (if (and rem (= "invoke" mtd))
                          foa
                          (let [fc (first (filter #(re-matches (re-pattern (str "^" % "\\..*")) cls)
                                                  *on-lookup-order*))]
                            (if (and fc (not (get (first foa) fc)))
                              [(assoc (first foa) fc ste) (conj (second foa) ste)]
                              foa)))))
                    [{} []]
                    (seq (.getStackTrace e)))]
    #_(lg "fos:" fos)
    (if *prefer-on-lookup-order*
      (when-not (empty? (first fos))
        (loop [elo *on-lookup-order*]
          (if-let [ste (get (first fos) (first elo))]
            ste
            (recur (rest elo)))))
      (when-not (empty? (second fos))
        (first (second fos))))))

(defn handle-other-exceptions
  [#^Exception e _]
  (when-let [on-ste (find-on-ste e)]
    (let [cls (.getClassName on-ste)
          rem (re-matches #"^([^$]+)\$.*" cls)]
      (when rem
        (let [fqon (second rem)
              fname (.getFileName on-ste)
              line (.getLineNumber on-ste)]
          (launch-exceptor e fqon fname line))))))
