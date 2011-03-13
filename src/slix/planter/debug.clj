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

(ns slix.planter.debug
  (:use [sevenri config core log]
        [slix.planter core io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *opwd* "")

(def *lbs* (java.io.ByteArrayOutputStream.))
(def *los* (java.io.OutputStreamWriter. (java.io.PrintStream. *lbs*)))

(def *abs* (java.io.ByteArrayOutputStream.))
(def *aos* (java.io.PrintStream. *abs*))

(defmacro def-out-ps
  [n]
  (let [[baos ops] (get-out-ps)
        baosn (symbol (format "baos%d" n))
        opsn (symbol (format "ops%d" n))]
    `(do
       (def ~baosn ~baos)
       (def ~opsn ~ops))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (is-project-built? 'slix.planter)
  (lg "planter: debug: reloading lacet and leiningen")
  (require 'lancet :reload)
  (require 'lancet.core :reload)
  (use 'leiningen.core :reload))

(defn set-pwd
  [pwd]
  (when (and (string? pwd) (not (empty? pwd)) (.exists (java.io.File. pwd)))
    (def *opwd* pwd)))

(defn print-lein-out
  []
  (.toString *lbs*))

(defn clear-lein-out
  []
  (.reset *lbs*))

(defn print-ant-out
  []
  (.toString *abs*))

(defn clear-ant-out
  []
  (.reset *abs*))

(defmacro lein
  [tname & args]
  `(let [tname# (str ~tname)
         args# (when ~args (map str ~args))]
     (when (and (string? *opwd*)
                (not (empty? *opwd*))
                (.exists (java.io.File. *opwd*)))
       (clear-lein-out)
       (clear-ant-out)
       (binding [*out* *los*
                 *original-pwd* *opwd*
                 lancet/ant-project (get-ant-project *aos* *aos*)
                 lancet.core/ant-project (get-ant-project *aos* *aos*)]
         (apply -main tname# args#))
       (println "=== ant output ===\n" (print-ant-out))
       (println "=== lein output ===\n" (print-lein-out)))))
