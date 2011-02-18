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

(ns openar.defs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main

(def quit-lock (proxy [Object][]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; config

(def *dop-dir* nil)

(defn reset-dop-dir
  [dir]
  (def *dop-dir* dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; debug

(def *swank-repl-is-running* false)

(defn reset-swank-repl-state
  [running?]
  (def *swank-repl-is-running* running?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jvm

(def *Sun-awt-is-available* (if (and (Class/forName "sun.awt.AppContext")
                                     (Class/forName "sun.awt.SunToolkit"))
                              true
                              false))

;;;;

(def *system-app-context* nil)

(defn reset-system-app-context
  [app-context]
  (def *system-app-context* app-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; log

(def *openar-logger-name* "Openar-logger")
(def *openar-logger-header* "openar:")
(def *openar-logger* nil)

(defn reset-openar-logger
  [name header logger]
  (def *openar-logger-name* name)
  (def *openar-logger-header* header)
  (def *openar-logger* logger))

(def *openar-logger-popup-level* java.util.logging.Level/WARNING)
(def *openar-logger-popup-sec* 3)

(defn reset-openar-logger-popup
  ([level]
     (def *openar-logger-popup-level* level))
  ([level sec]
     (def *openar-logger-popup-level* level)
     (def *openar-logger-popup-sec* sec)))

(defn get-openar-logger-popup-level
  []
  (cond
   (instance? java.util.logging.Level *openar-logger-popup-level*)
     (.intValue *openar-logger-popup-level*)
   (instance? Integer *openar-logger-popup-level*)
     *openar-logger-popup-level*
   :else
     (.intValue java.util.logging.Level/OFF)))

;;;;

(def *openar-log-file* nil)

(defn reset-openar-log-file
  [log-file]
  (def *openar-log-file* log-file))

;;;;

(def *standard-in*  nil)
(def *standard-out* nil)
(def *standard-err* nil)

(defn reset-standard-in-out-err
  [in out err]
  (def *standard-in* in)
  (def *standard-out* out)
  (def *standard-err* err))

;;;;

(def *thread-default-uncaught-exception-handler* nil)

(defn reset-thread-default-uncaught-exception-handler
  [handler]
  (def *thread-default-uncaught-exception-handler* handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main

(def *ok-to-quit-fn* nil)

(defn reset-ok-to-quit-fn
  [ok-to-quit-fn]
  (def *ok-to-quit-fn* ok-to-quit-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; oplix

(def *set-location-by-platform* true)

(defn set-location-by-platform
  [by-platform?]
  (def *set-location-by-platform* by-platform?))

(def *base-class-loader* nil)

(defn reset-base-class-loader
  [loader]
  (def *base-class-loader* loader))

(def *oplix-openar-can-close* false)

(defn oplix-openar-can-close
  [can?]
  (def *oplix-openar-can-close* can?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup

;; The order is important. Don't change unless it's necessary.
(def *startup-order* '[log jvm os core oplix ui])

(defn using-fn-prefix
  [fquns]
  (let [uns (if-let [m (re-matches #"^openar\.(.*)$" (str fquns))]
              (second m)
              fquns)]
    (str uns "-using-")))

(defmacro using-fns
  "uns is using rfns of rns. Expand to defns of uns-using-rfn-uns"
  [uns rns rfns]
  (let [ufp (using-fn-prefix uns)
        dfs (map #(list 'defn
                        (symbol (str ufp % \- rns))
                        '[& args]
                        (vector (list 'quote %) (list 'quote rns)))
                 rfns)]
    `(do ~@dfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ui

(def *saved-dynaclass-listeners* '_*edl*_)
