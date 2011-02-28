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

;; Thanks to:
;; John Gruber, http://daringfireball.net/projects/markdown/
;; Brian Carper, http://briancarper.net/blog/415/clojure-and-markdown-and-javascript-and-java-and
;; John Fraser, http://attacklab.net/showdown/
;; The mozilla Foundation and its community, http://www.mozilla.org/rhino/

(ns slix.documenter.md2html
  (:use [sevenri log slix])
  (:import (org.mozilla.javascript Context ScriptableObject)))

(defn markdown-to-html [txt]
  (let [cx (Context/enter)
        scope (.initStandardObjects cx)
        input (Context/javaToJS txt scope)
        script (str (slurp (str (get-slix-dir 'documenter) "/showdown/showdown.js"))
                    "new Showdown.converter().makeHtml(input);")]
    (try
      (ScriptableObject/putProperty scope "input" input)
      (let [result (.evaluateString cx scope script "<cmd>" 1 nil)]
        (Context/toString result))
      (catch Exception e
        (log-exception e)
        nil)
      (finally (Context/exit)))))
