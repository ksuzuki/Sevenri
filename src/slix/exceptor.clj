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

(ns ^{:slix true :exception-listener 'handle-exception}
  slix.exceptor
  (:use [sevenri event log slix]
        [slix.exceptor core defs edb handler ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-exception
  [#^Exception e fqsn]
  #_(lg "exceptor: class of e:" (class e) "e.msg:" (.getMessage e) "fqsn:" fqsn)
  (if (and (instance? clojure.lang.Compiler$CompilerException e) fqsn)
    (handle-sn-compiler-exception e fqsn)
    (handle-other-exceptions e fqsn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn opening
  [event]
  (register-exception-handler)
  (let [ge (get-last-global-event)]
    (when-not (and (:exception (slix-args))
                   (:file (slix-args))
                   (:line (slix-args))
                   (not= ge :sevenri.event/slixes-closing)
                   (not= ge :sevenri.event/sevenri-quitting))
      (create-event-response
       :sevenri.event/response-donot-open
       :missing-required-arg))))

(defn frame-created
  [event]
  (add-ui))

(defn opened
  [event]
  (set-slix-visible)
  (when (:open-ced (slix-args))
    (invoke-later #(do
                     (deref (open-slix-with-args
                             {:file (:file (slix-args))
                              :line (:line (slix-args))}
                             *ced*))
                     (.toFront (slix-frame))))))

(defn saving
  [event]
  (event-response-donot-save))

(defn closing
  [event]
  (remove-from-edb (:file (slix-args)) (:line (slix-args))))
