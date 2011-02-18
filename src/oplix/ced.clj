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

(ns ^{:oplix true}
  oplix.ced
  (:use [openar event log oplix]
        [oplix.ced init ui utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; oplix event handlers

(defn opening
  [event]
  (let [file (get-file)
        ocfs (filter (fn [[o cf]] (= cf file)) (xref-with :ced-file))]
    (when (seq ocfs)
      (let [oplix (ffirst ocfs)
            args (oplix-args)]
        (invoke-later oplix
          #(do
             (.toFront (oplix-frame))
             (process-args (get-ced) args)))
        (create-event-response
         :openar.event/response-donot-open
         :file-in-editing-already)))))

(defn frame-created
  [event]
  (create-ced-frame))

(defn opened
  [event]
  (load-file*)
  (initial-setup)
  (set-oplix-visible))

(defn saving
  [event]
  (event-response-donot-save))

(defn closing
  [event]
  (let [doc (.getDocument (get-ced))]
    (when (.isModified doc)
      ;; Cannot deny closing and ask for save with these conditions
      ;; openar.event/info-close-on-delete is true
      ;; openar.event/oplixes-closing
      (cond
       (true? (:openar.event/info-close-on-delete (get-event-info event)))
         (.save doc)
       (= (get-last-global-event) :openar.event/oplixes-closing)
         (.save doc)
       :else
         (do
           (invoke-later #(ask-then-close doc))
           :openar.event/response-donot-close)))))
