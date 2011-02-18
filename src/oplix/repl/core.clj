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

(ns oplix.repl.core
  (:require clojure.main)
  (:use [openar config event log oplix ui utils]
        [oplix.repl defs listeners])
  (:import (java.io File OutputStreamWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *startup-script* 'replrc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-startup-script-file-name
  []
  *startup-script*)

(defn get-startup-script-file
  []
  (get-oplix-file 'repl (get-startup-script-file-name)))

(defmacro repl
  [& args]
  (if (seq args)
    (let [arg1# (first args)]
      (cond
       (symbol? arg1#)
         `(open-oplix-with-args (str '~arg1#) ~''repl)
       (string? arg1#)
         `(open-oplix-with-args ~arg1# ~''repl)
       :else
         `(if (is-oplix? ~arg1#)
             (open-oplix-with-args (oplix-name ~arg1#) ~''repl)
             (open-oplix ~''repl))))
    `(open-oplix ~''repl)))

(defn setup-replrc
  "Copy the default startup script to the repl-oplix's directory if a
   startup script doesnot exist. Return the script's file path, or if
   alt-open is specified in the args, reutn the file path of the default
   startup script."
  [repl-oplix]
  (let [src (get-startup-script-file)
        tgt (with-create-on-get-dir
              (File. (get-dop-oplix-dir (oplix-on repl-oplix)) (str (get-startup-script-file-name) ".clj")))]
    (when-not (.exists tgt)
      (clojure.java.io/copy src tgt))
    (if (and (map? (oplix-args)) ((get-default :oplix :arguments :alt-open) (oplix-args)))
      src
      tgt)))

(defn start-repl*
  [oplix repl-oplix con ins rrc]
    (binding [*in*  (clojure.lang.LineNumberingPushbackReader. (.getIn con))
              *out* (OutputStreamWriter. (.getOut con))
              *err* (.getOut con)
              *oplix* repl-oplix]
      (let [it (fn []
                 (in-ns ins)
                 (use '[openar config core debug event jvm log oplix os ui utils]
                      '[oplix.repl.core :only (repl clear-repl-content)])
                 (when (pos? (.getLength (.getDocument con))) (println))
                 (try
                   (require 'clojure.stacktrace)
                   (when (.exists rrc) (load-file (str rrc)))
                   (catch Exception e (clojure.stacktrace/print-stack-trace e))))
            pf (fn []
                 (let [np (str *ns* "=>")]
                   (when-not (= np (.getPromptString con))
                     (set-oplix-title oplix (format "%s - %s" (oplix-name oplix) (str *ns*))))
                   (.setPromptString con np)
                   (.write *out* (.getPromptCharacter con))))
            rf (bound-fn [] (clojure.main/repl :init it :prompt pf))]
        (invoke-later repl-oplix #(let [rt (Thread. rf)]
                                    (put-oplix-prop oplix :repl-thread rt)
                                    (.start rt))))))

(defn start-repl
  "When the argment is an oplix name, start in the namespace of the oplix.
   Otherwise, start in the user namespace."
  ([]
     (let [args (oplix-args)]
       (start-repl (if (or (symbol? args) (string? args))
                     args
                     (oplix-name)))))
  ([repl-oplix-name]
     (if-let [repl-oplix (get-oplix repl-oplix-name)]
       ;; Specified oplix is found.
       (start-repl repl-oplix-name repl-oplix)
       ;; Not found. Fallback to this oplix.
       (start-repl (oplix-name) *oplix*)))
  ([repl-oplix-name repl-oplix]
     (let [op *oplix*
           fr (oplix-frame)
           cn (.getComponent (.getContentPane fr) 0)
           tp (.getTextPane cn)
           sd (.getDocument cn)
           rc (setup-replrc repl-oplix)
           in (let [opon (oplix-on repl-oplix)]
                (if (= opon 'repl)
                  'user
                  (get-oplix-fqns opon)))
           sr #(start-repl* op repl-oplix cn in rc)]
       ;; Remember start-in namespace, replrc, and console.
       (put-oplix-prop :inns in)
       (put-oplix-prop :replrc rc)
       (put-oplix-prop :console cn)
       ;; Add listeners to textpane.
       (add-listeners tp sd cn sr op)
       ;; Start repl.
       (sr))))

(defn saving-repl
  [event]
  (if (= (oplix-name) "Repl")
    ;; save only 'Repl' and don't report any XMLEncoder errors.
    (do
      (when (is-event-info-save-on-close? event)
        (.suspend (get-oplix-prop :repl-thread)))
      (save-dyna-listeners
       [[(oplix-frame) [(listener-triplet Window)]]
        [(.getTextPane (get-oplix-prop :console)) [(listener-triplet Caret)
                                                   (listener-triplet Key)]]])
      (create-event-response :openar.event/response-suppress-xml-encoder-errors))
    ;; Discard others.
    (event-response-donot-save)))

(defn saved-repl
  [event]
  (when (and (= (oplix-name) "Repl")
             (is-event-info-save-on-close? event))
    (.resume (get-oplix-prop :repl-thread))))

(defn end-repl
  [event]
  (.stop (get-oplix-prop :repl-thread))
  (.closeIO (get-oplix-prop :console)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-repl-content
  []
  (when-let [console (get-oplix-prop :console)]
    (.setText (.getTextPane console) "")))
