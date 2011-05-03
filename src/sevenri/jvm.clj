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

(ns ^{:doc "Sevenri interface library to Java VM depedent features"}
  sevenri.jvm
  (:use [sevenri config defs log props])
  (:import (java.awt EventQueue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-Sun-awt-available?
  []
  *Sun-awt-is-available*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro invoke-sun-awt-static-method
  [^String class ^String method]
  `(.invoke (.getDeclaredMethod (Class/forName ~class) ~method nil) nil nil))

(defn get-app-context
 []
 (when *Sun-awt-is-available*
   (invoke-sun-awt-static-method "sun.awt.AppContext" "getAppContext")))

(defn get-app-contexts
 []
 (when *Sun-awt-is-available*
   (invoke-sun-awt-static-method "sun.awt.AppContext" "getAppContexts")))

(defn get-system-app-context
  []
  *system-app-context*)

(defn create-app-context
  ([slix-name slix-class-loader]
     (when *Sun-awt-is-available*
       (let [system-thread-group (.getThreadGroup (get-system-app-context))
             slix-thread-group (ThreadGroup. system-thread-group (str slix-name))
             app-context (atom nil)
             app-context-creator (Thread. slix-thread-group
                                          (proxy [Runnable] []
                                            (run []
                                                 (create-app-context app-context))))]
         (doto app-context-creator
           (.setContextClassLoader slix-class-loader)
           (.start)
           (.join))
         @app-context)))
  ([app-context]
     (when *Sun-awt-is-available*
       (reset! app-context (invoke-sun-awt-static-method "sun.awt.SunToolkit" "createNewAppContext")))))

(defn dispose-app-context
  [app-context]
  (.dispose app-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn invoke-later-in-slix-context
  ":app-context must be available in slix's :context."
  [slix cljfn wait?]
  (when *Sun-awt-is-available*
    (let [bound-cljfn (bound-fn [] (cljfn))
          cljfn-runner (proxy [Runnable] []
                         (run []
                              (try
                                (bound-cljfn)
                                (catch Exception e
                                  (log-exception e)))))
          app-context (:app-context (:context slix))
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

(defn alt-invoke-later-in-slix-context
  "Alternative of invoke-later-in-slix-context when app-context isn't
   available. Per-slix class loader should be available with slix."
  ([slix cljfn]
     (alt-invoke-later-in-slix-context slix cljfn false))
  ([slix cljfn wait?]
     (let [bound-cljfn (bound-fn [] (cljfn))
           cljfn-runner (proxy [Runnable] []
                          (run []
                               (let [ct (Thread/currentThread)
                                     ccl (.getContextClassLoader ct)]
                                 (try
                                   (.setContextClassLoader ct (or (:cl slix) ccl))
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
  (save-prop (get-props) "sun.awt.exception.handler" awt-exception-handler-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn- -acquire-system-app-context?
  []
  (when-not *system-app-context*
    (reset-system-app-context (get-app-context)))
  true)

(defn- -register-awt-exception-handler?
  []
  (register-awt-exception-handler (get-config 'src.sevenri.listeners.awtehandler))
  true)

;;;;

(defn startup-jvm?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -acquire-system-app-context?
          -register-awt-exception-handler?)))

(defn shutdown-jvm?
  []
  (apply while-each-true?
         nil))
