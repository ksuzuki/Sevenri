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

(ns slix.repl.core
  (:require clojure.main)
  (:use [sevenri event props slix ui]
        [slix.repl listeners])
  (:import (java.io File OutputStreamWriter PrintWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *startup-script* 'replrc!clj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-startup-script-file-name
  []
  *startup-script*)

(defn get-startup-script-file
  []
  (get-slix-path 'repl (get-startup-script-file-name)))

(defmacro repl
  [& args]
  (if (seq args)
    (let [arg1# (first args)]
      (cond
       (symbol? arg1#)
         `(open-slix-with-args (str '~arg1#) ~''repl)
       (string? arg1#)
         `(open-slix-with-args ~arg1# ~''repl)
       :else
         `(if (is-slix? ~arg1#)
             (open-slix-with-args (slix-name ~arg1#) ~''repl)
             (open-slix ~''repl))))
    `(open-slix ~''repl)))

(defn setup-replrc
  "Copy the default startup script to the repl-slix's directory if a
   startup script doesnot exist. Return the script's file path, or if
   alt-open is specified in the args, reutn the file path of the default
   startup script."
  [repl-slix]
  (let [src (get-startup-script-file)
        tgt (get-sid-slix-path (slix-sn repl-slix) (get-startup-script-file-name))]
    (when-not (.exists tgt)
      (clojure.java.io/copy src tgt))
    (if (alt-open-slix?)
      src
      tgt)))

(defn start-repl*
  [slix repl-slix con ins rrc]
    (binding [*in*  (clojure.lang.LineNumberingPushbackReader. (.getIn con))
              *out* (OutputStreamWriter. (.getOut con))
              *err* (PrintWriter. (OutputStreamWriter. (.getOut con)) true)
              *slix* repl-slix]
      (let [it (fn []
                 (in-ns ins)
                 (use '[sevenri config core defs log slix props])
                 (when (pos? (.getLength (.getDocument con))) (println))
                 (try
                   (require 'clojure.stacktrace)
                   (when (.exists rrc) (load-file (str rrc)))
                   (catch Exception e (clojure.stacktrace/print-stack-trace e))))
            pf (fn []
                 (let [np (str *ns* "=>")]
                   (when-not (= np (.getPromptString con))
                     (set-slix-title slix (format "%s - %s" (slix-name slix) (str *ns*))))
                   (.setPromptString con np)
                   (.write *out* (.getPromptCharacter con))))
            rf (bound-fn [] (clojure.main/repl :init it :prompt pf))]
        (invoke-later repl-slix #(let [rt (Thread. rf)]
                                    (put-prop (slix-props slix) 'repl.thread rt)
                                    (.start rt))))))

(defn start-repl
  "When the argment is a slix name, start in the namespace of the slix.
   Otherwise, start in the user namespace."
  ([]
     (let [args (slix-args)]
       (start-repl (if (or (symbol? args) (string? args))
                     args
                     (slix-name)))))
  ([repl-slix-name]
     (if-let [repl-slix (get-slix repl-slix-name)]
       ;; Specified slix is found.
       (start-repl repl-slix-name repl-slix)
       ;; Not found. Fallback to this slix.
       (start-repl (slix-name) *slix*)))
  ([repl-slix-name repl-slix]
     (let [sx *slix*
           fr (slix-frame)
           cn (.getComponent (.getContentPane fr) 0)
           tp (.getTextPane cn)
           sd (.getDocument cn)
           rc (setup-replrc repl-slix)
           in (let [sxsn (slix-sn repl-slix)]
                (if (= sxsn 'repl)
                  'user
                  (get-slix-ns sxsn)))
           sr #(start-repl* sx repl-slix cn in rc)]
       ;; Remember console.
       (put-prop (slix-props) 'console cn)
       ;; Add listeners to textpane.
       (add-listeners tp sd cn sr sx)
       ;; Start repl.
       (sr))))

(defn saving-repl
  "Save only 'Repl' and discard other repls."
  [event]
  (if (= (slix-name) "Repl")
    (when (is-event-info-save-on-close? event)
      (.suspend (get-prop (slix-props) 'repl.thread)))
    (event-response-donot-save)))

(defn saved-repl
  [event]
  (when (and (= (slix-name) "Repl")
             (is-event-info-save-on-close? event))
    (.resume (get-prop (slix-props) 'repl.thread))))

(defn end-repl
  [event]
  ;; CloseIO then stop repl thread to suppress unnecessary ThreadDeath
  ;; message printed in repl window.
  (.closeIO (get-prop (slix-props) 'console))
  (.stop (get-prop (slix-props) 'repl.thread)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-repl-content
  []
  (when-let [console (get-prop (slix-props) 'console)]
    (.setText (.getTextPane console) "")))
