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

(ns slix.api-browser.core
  (:use [sevenri config core log slix])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-api-doc-urls?
  []
  (let [f (get-library-slix-path 'api-browser (get-config 'doc.apidoc-urls-file-name))]
    (if (.exists f)
      (try
        (load-file (str f))
        true
        (catch Exception e
          (log-exception e)
          false))
      (do
        (log-warning "api-browser: not found:" f)
        false))))

(defn trim-api-keyword
  [api-keyword]
  (let [akwd0 (if-let [match (re-matches #"^\.+(.*)$" api-keyword)]
                (second match)
                api-keyword)
        akwd1 (if-let [match (re-matches #"^(.*)\.+$" akwd0)]
                (second match)
                akwd0)
        [kwd-ns kwd-name] (if-let [match (re-matches #"^(.*)\.([^.]+)$" akwd1)]
                            [(second match) (last match)]
                            [nil akwd1])]
    [kwd-ns (.replaceAll kwd-name "\\$" ".")]))
