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

(ns ^{:slix true :singleton true}
  slix.planter
  (:use [sevenri config core event log slix ui utils]
        [slix.planter core init]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query I/F for manager

(defmulti ready?
  (fn [obj] (class obj)))

(defmethod ready? clojure.lang.PersistentArrayMap
  [m]
  (is-manager-ready? (:slix-name m)))

(defmethod ready? :default
  [slix-name]
  (ready? {:slix-name slix-name}))

;;;;

(defmulti setup?
  (fn [obj] (class obj)))

(defmethod setup? clojure.lang.PersistentArrayMap
  [m]
  (setup-manager? (:slix-name m)))

(defmethod setup? :default
  [slix-name]
  (setup? {:slix-name slix-name}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query I/F for project

(defmulti exists?
  (fn [obj] (class obj)))

(defmethod exists? clojure.lang.PersistentArrayMap
  [m]
  (project-exists? (:slix-name m)))

(defmethod exists? :default
  [slix-name]
  (exists? {:slix-name slix-name}))

;;;;

(defmulti built?
  (fn [obj] (class obj)))

(defmethod built? clojure.lang.PersistentArrayMap
  [m]
  (is-project-built? (:slix-name m)))

(defmethod built? :default
  [slix-name]
  (built? {:slix-name slix-name}))

;;;;

(defmulti build?
  (fn [obj] (class obj)))

(defmethod build? clojure.lang.PersistentArrayMap
  [m]
  (build-project? (:slix-name m)))

(defmethod build? :default
  [slix-name]
  (build? {:slix-name slix-name}))

;;;;

(defmulti get-jars
  (fn [obj] (class obj)))

(defmethod get-jars clojure.lang.PersistentArrayMap
  [m]
  (get-project-all-jars (:slix-name m)))

(defmethod get-jars :default
  [slix-name]
  (get-jars {:slix-name slix-name}))

;;;;

(defn build-and-run
  ([m]
     (build-slix-project-and-run (:slix-name m) (:name m) (:arguments m)))
  ([slix-name name args]
     (build-and-run {:slix-name slix-name :name name :arguments args})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn opened
  [event]
  (set-slix-visible)
  (verify))
