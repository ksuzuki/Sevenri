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

(ns ^{:oplix true :exception-listener 'handle-exception}
  oplix.exceptor
  (:use [openar event log oplix]
        [oplix.exceptor core defs edb handler ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-exception
  [#^Exception e fqon]
  #_(lg "exceptor: class of e:" (class e) "e.msg:" (.getMessage e) "fqon:" fqon)
  (if (and (instance? clojure.lang.Compiler$CompilerException e) fqon)
    (handle-on-compiler-exception e fqon)
    (handle-other-exceptions e fqon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn opening
  [event]
  (register-exception-handler)
  (let [ge (get-last-global-event)]
    (when-not (and (:exception (oplix-args))
                   (:file (oplix-args))
                   (:line (oplix-args))
                   (not= ge :openar.event/oplixes-closing)
                   (not= ge :openar.event/openar-quitting))
      (create-event-response
       :openar.event/response-donot-open
       :missing-required-arg))))

(defn frame-created
  [event]
  (add-ui))

(defn opened
  [event]
  (set-oplix-visible)
  (when (:open-ced (oplix-args))
    (invoke-later #(do
                     (deref (open-oplix-with-args
                             {:file (:file (oplix-args))
                              :line (:line (oplix-args))}
                             *ced*))
                     (.toFront (oplix-frame))))))

(defn saving
  [event]
  (event-response-donot-save))

(defn closing
  [event]
  (remove-from-edb (:file (oplix-args)) (:line (oplix-args))))
