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
        [slix.planter core defs io]
        [slix.planter.init :only [load-lein-core]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *opwd* "")

(def *abs* (java.io.ByteArrayOutputStream.))
(def *aos* (java.io.PrintStream. *abs*))

(def *lbs* (java.io.ByteArrayOutputStream.))
(def *los* (java.io.OutputStreamWriter. (java.io.PrintStream. *lbs* true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (is-project-built? 'slix.planter)
  (load-lein-core))

(defn set-pwd
  [pwd]
  (when (and (string? pwd) (not (empty? pwd)) (.exists (java.io.File. pwd)))
    (def *opwd* pwd)))

(defn print-ant-out
  []
  (.toString *abs*))

(defn clear-ant-out
  []
  (.reset *abs*))

(defn print-lein-out
  []
  (.toString *lbs*))

(defn clear-lein-out
  []
  (.reset *lbs*))

(defmacro lein
  [tname & args]
  `(let [tname# (str ~tname)
         args# (when ~args (map str ~args))]
     (when (and (string? *opwd*)
                (not (empty? *opwd*))
                (.exists (java.io.File. *opwd*)))
       (clear-ant-out)
       (clear-lein-out)
       (binding [clojure.core/*out* *los*
                 leiningen.core/*original-pwd* *opwd*
                 leiningen.core/*eval-in-lein* false
                 leiningen.core/*exit* false
                 lancet/ant-project (get-ant-project *aos* *aos*)
                 lancet.core/ant-project (get-ant-project *aos* *aos*)]
         (apply leiningen.core/-main tname# args#))
       (println "=== ant output ===\n" (print-ant-out))
       (println "=== lein output ===\n" (print-lein-out)))))
