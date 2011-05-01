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

(ns slix.exceptor.handler
  (:use [sevenri log]
        [slix.exceptor core defs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-sn-compiler-exception
  [^Exception e fqsn]
  (let [cmsg (.getMessage e)
        mtch (re-matches #".*\(([^:]+):(\d+)\)$" cmsg)]
    #_(lg "fqsn:" fqsn "cmsg:" cmsg "mtch:" mtch "(rest mtch):" (rest mtch))
    (when mtch
      (let [[fname line] (rest mtch)]
        (launch-exceptor e fqsn fname line)))))

(defn find-sn-ste
  [^Exception e]
  (let [fss (reduce (fn [fsa ste]
                      (let [cls (.getClassName ste)
                            mtd (.getMethodName ste)
                            rem (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" cls)]
                        (if (and rem (= "invoke" mtd))
                          fsa
                          (let [fc (first (filter #(re-matches (re-pattern (str "^" % "\\..*")) cls)
                                                  *sn-lookup-order*))]
                            (if (and fc (not (get (first fsa) fc)))
                              [(assoc (first fsa) fc ste) (conj (second fsa) ste)]
                              fsa)))))
                    [{} []]
                    (seq (.getStackTrace e)))]
    #_(lg "fss:" fss)
    (if *prefer-sn-lookup-order*
      (when-not (empty? (first fss))
        (loop [elo *sn-lookup-order*]
          (if-let [ste (get (first fss) (first elo))]
            ste
            (recur (rest elo)))))
      (when-not (empty? (second fss))
        (first (second fss))))))

(defn handle-other-exceptions
  [^Exception e _]
  (when-let [sn-ste (find-sn-ste e)]
    (let [cls (.getClassName sn-ste)
          rem (re-matches #"^([^$]+)\$.*" cls)]
      (when rem
        ;; Note that the fully-qualified slix name in the rem is actually a
        ;; Java package name. So only the translation from '_' to '-' to it
        ;; is required.
        (let [fqsn (.replace (second rem) \_ \-)
              fname (.getFileName sn-ste)
              line (.getLineNumber sn-ste)]
          (launch-exceptor e fqsn fname line))))))
