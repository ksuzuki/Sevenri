(ns ^{:slix true}
  slix.planter
  (:use [sevenri config core event log slix ui utils]
        [slix.planter core]))

(defn opened
  [event]
  (set-slix-visible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn built?
  [m]
  (is-project-built? (:project-name m)))

(defn build?
  [m]
  (build-project? (:project-name m)))

(defn get-jars
  [m]
  (get-project-jars (:project-name m)))
