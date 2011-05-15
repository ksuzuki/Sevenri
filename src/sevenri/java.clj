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

(ns sevenri.java
  "Sevenri Java utility lib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-class-fields
  [^java.lang.Class class]
  (apply hash-map (flatten (map #(vector (.getName %) %) (.getFields class)))))

(defn get-class-field
 [^java.lang.Class class field-name]
 (get (get-class-fields class) (str field-name)))

(defn get-class-field-value
  ([^java.lang.Class class field-name]
     (get-class-field-value class field-name nil))
  ([^java.lang.Class class field-name not-found]
     (if-let [f (get-class-field class field-name)]
       (.get f class)
       not-found)))
