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

(ns openar.jvm
  (:use [openar config defs log])
  (:import (java.awt EventQueue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-Sun-awt-available?
  []
  *Sun-awt-is-available*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-app-context
 []
 (when *Sun-awt-is-available*
   (sun.awt.AppContext/getAppContext)))

(defn get-app-contexts
 []
 (when *Sun-awt-is-available*
   (sun.awt.AppContext/getAppContexts)))

(defn get-system-app-context
  []
  *system-app-context*)

(defn create-app-context
  ([oplix-name oplix-class-loader]
     (when *Sun-awt-is-available*
       (let [system-thread-group (.getThreadGroup (get-system-app-context))
             oplix-thread-group (ThreadGroup. system-thread-group (str oplix-name))
             app-context (atom nil)
             app-context-creator (Thread. oplix-thread-group
                                          (proxy [Runnable] []
                                            (run []
                                                 (create-app-context app-context))))]
         (doto app-context-creator
           (.setContextClassLoader oplix-class-loader)
           (.start)
           (.join))
         @app-context)))
  ([app-context]
     (when *Sun-awt-is-available*
       (reset! app-context (sun.awt.SunToolkit/createNewAppContext)))))

(defn dispose-app-context
  [app-context]
  (.dispose app-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn invoke-later-in-oplix-context
  ":app-context must be available in oplix's :context."
  [oplix cljfn wait?]
  (when *Sun-awt-is-available*
    (let [bound-cljfn (bound-fn [] (cljfn))
          cljfn-runner (proxy [Runnable] []
                         (run []
                              (try
                                (bound-cljfn)
                                (catch Exception e
                                  (log-exception e)))))
          app-context (:app-context (:context oplix))
          ac-thread-group (.getThreadGroup app-context)
          thread-in-ac (Thread. ac-thread-group (proxy [Runnable] []
                                                  (run []
                                                       (if wait?
                                                         (EventQueue/invokeAndWait cljfn-runner)
                                                         (EventQueue/invokeLater cljfn-runner)))))]
      (doto thread-in-ac
        (.start)
        (.join)) ;; join here and wait for returning from EventQueue/invokeAndWait.
      thread-in-ac)))

(defn alt-invoke-later-in-oplix-context
  "Alternative of invoke-later-in-oplix-context when app-context isn't
   available. Per-oplix class loader should be available with oplix."
  ([oplix cljfn]
     (alt-invoke-later-in-oplix-context oplix cljfn false))
  ([oplix cljfn wait?]
     (let [bound-cljfn (bound-fn [] (cljfn))
           cljfn-runner (proxy [Runnable] []
                          (run []
                               (let [ct (Thread/currentThread)
                                     ccl (.getContextClassLoader ct)]
                                 (try
                                   (.setContextClassLoader ct (or (:cl oplix) ccl))
                                   (bound-cljfn)
                                   (catch Exception e
                                     (log-exception e))
                                   (finally
                                    (.setContextClassLoader ct ccl))))))]
       (if wait?
         (EventQueue/invokeAndWait cljfn-runner)
         (EventQueue/invokeLater cljfn-runner)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn register-awt-exception-handler
  [awt-exception-handler-name]
  (System/setProperty "sun.awt.exception.handler" (str awt-exception-handler-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -acquire-system-app-context?
  []
  (when-not *system-app-context*
    (reset-system-app-context (get-app-context)))
  true)

(defn register-awt-exception-handler?
  []
  (register-awt-exception-handler (get-default :src :openar :listeners :awtehandler))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn shutdown-jvm?
  []
  true)

(defn startup-jvm?
  []
  (and true
       (-acquire-system-app-context?)
       (register-awt-exception-handler?)))
