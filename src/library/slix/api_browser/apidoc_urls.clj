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

(in-ns 'slix.api-browser.java)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Return root which should end with '/'.

(defn get-net-java-docs-api-url-for-se*
  []
  "http://download.oracle.com/javase/6/docs/api/")

(defn get-net-java-docs-api-url-for-ee*
  []
  "http://download.oracle.com/javaee/6/api/")

(defn get-local-java-docs-api-url*
  [sdk-name-ver]
  (let [lp (get-doc-path 'apidocs sdk-name-ver 'docs.api)]
    (when (.exists lp)
      (str "file://" lp "/"))))

;;;;

(defn get-java-docs-api-url-for-se*
  []
  (or (get-local-java-docs-api-url* 'J2SE.6!0)
      (get-net-java-docs-api-url-for-se*)))

(defn get-java-docs-api-url-for-ee*
  []
  (or (get-local-java-docs-api-url* 'JavaEE.6!0)
      (get-net-java-docs-api-url-for-ee*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Class index html

(defn get-java-docs-api-urls
  []
  [[(get-java-docs-api-url-for-se*) "allclasses-noframe.html"]
   [(get-java-docs-api-url-for-ee*) "allclasses-noframe.html"]])
