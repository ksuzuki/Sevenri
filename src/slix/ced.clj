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

(ns ^{:slix true}
  slix.ced
  (:use [sevenri event log slix]
        [slix.ced init ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn opening
  [event]
  (let [file (get-ced-file)
        ocfs (filter (fn [[o cf]] (= cf file)) (xref-with :ced-file))]
    (when (seq ocfs)
      (let [slix (ffirst ocfs)
            args (slix-args)]
        (invoke-later slix
          #(do
             (.toFront (slix-frame))
             (process-args (get-ced) args)))
        (create-event-response
         :sevenri.event/response-donot-open
         :file-in-editing-already)))))

(defn frame-created
  [event]
  (create-ced-frame))

(defn opened
  [event]
  (load-ced-file)
  (initial-setup)
  (set-slix-visible))

(defn saving
  [event]
  (event-response-donot-save))

(defn closing
  [event]
  (let [doc (.getDocument (get-ced))]
    (when (.isModified doc)
      ;; Cannot deny closing and ask for save with these conditions
      ;; sevenri.event/info-close-on-delete is true
      ;; sevenri.event/slixes-closing
      (cond
       (true? (:sevenri.event/info-close-on-delete (get-event-info event)))
         (.save doc)
       (= (get-last-global-event) :sevenri.event/slixes-closing)
         (.save doc)
       :else
         (do
           (invoke-later #(ask-then-close doc))
           :sevenri.event/response-donot-close)))))
