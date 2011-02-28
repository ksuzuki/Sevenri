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

(ns slix.exceptor.edb
  (:use [sevenri log slix]
        [slix.exceptor refs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-edb
  []
  (send *edb* (fn [edb] {})))

(defn add-to-edb
  [#^Exception e file line open-ced?]
  (send *edb* (fn [edb]
                (let [launch? (if-let [l (get edb (str file))]
                                (not= l line)
                                true)]
                  (if launch?
                    (let [args (if open-ced?
                                 {:exception e :file file :line line :open-ced true}
                                 {:exception e :file file :line line})]
                      (open-slix-with-args args 'exceptor (gensym "Exceptor"))
                      (assoc edb (str file) line))
                    edb)))))

(defn remove-from-edb
  [file line]
  (send *edb* (fn [edb]
                (dissoc edb (str file) line))))

