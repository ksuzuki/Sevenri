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

(import 'java.io.File)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-ns 'slix.api-browser.java)

(defn get-local-java-docs-api-url*
  [sdk-name-ver]
  (let [lp (str (get-doc-dir) "/apidocs/" (str sdk-name-ver "/docs/api/"))]
    (when (.exists (File. lp))
      (str "file://" lp))))

(defn get-net-java-docs-api-url-for-se*
  []
  (let [url "http://download.oracle.com/javase/6/docs/api/"]
    url))

(defn get-net-java-docs-api-url-for-ee*
  []
  (let [url "http://download.oracle.com/javaee/6/api/"]
    url))

(defn get-java-docs-api-url-for-se*
  []
  (or (get-local-java-docs-api-url* "J2SE/6.0")
      (get-net-java-docs-api-url-for-se*)))

(defn get-java-docs-api-url-for-ee*
  []
  (or (get-local-java-docs-api-url* "JavaEE/6.0")
      (get-net-java-docs-api-url-for-ee*)))

;;;;

(defn get-java-docs-api-urls
  []
  [[(get-java-docs-api-url-for-se*) "allclasses-noframe.html"]
   [(get-java-docs-api-url-for-ee*) "allclasses-noframe.html"]])
