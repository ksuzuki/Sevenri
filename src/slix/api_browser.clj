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
  slix.api-browser
  (:use [sevenri event log slix]
        [slix.api-browser java ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn opening
  [event]
  (when-not (:keyword (slix-args))
    (create-event-response
     :sevenri.event/response-donot-open
     :missing-api-keyword)))

(defn frame-created
  [event]
  (create-api-browser-frame))

(defn opened
  [event]
  (let [api-kwd (:keyword (slix-args))]
    (invoke-later #(if (browse-java-api? (str api-kwd))
                     (close-slix (slix-name))
                     (set-slix-visible)))))

(defn saving
  [event]
  (event-response-donot-save))
