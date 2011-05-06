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

(ns ^{:doc "Sevenri debug library"}
  sevenri.debug
  (:use [sevenri config defs log props]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-swank-repl-running?
  []
  (read-prop (get-props) 'sevenri.debug.swank.running))

(defn run-swank-repl
  "Run swank-repl in a system thread."
  ([]
     (run-swank-repl (get-prop (get-props) 'sevenri.debug.swank.port)))
  ([port]
     (when-not (is-swank-repl-running?)
       (let [port (try
                    (Integer/parseInt (str port))
                    (catch Exception e
                      (Integer/parseInt (get-prop (get-props) 'sevenri.debug.swank.port))))
             encoding (get-prop (get-props) 'sevenri.debug.swank.encoding)
             ;;
             event-queue (.getSystemEventQueue (java.awt.Toolkit/getDefaultToolkit))
             source (Object.)
             runnable #(future
                         (put-prop (get-props) 'sevenri.debug.swank.running "true")
                         (require 'swank.swank)
                         ((ns-resolve 'swank.swank 'start-repl) port :encoding encoding))]
         (.postEvent event-queue (java.awt.event.InvocationEvent. source runnable))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn- -run-swank-repl?
  []
  (when-let [port (get-env (get-prop (get-props) 'sevenri.debug.envname))]
    (run-swank-repl port))
  true)

(defn startup-debug?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -run-swank-repl?)))

(defn shutdown-debug?
  []
  (apply while-each-true?
         nil))
