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
  (:use [sevenri config event log slix ui utils]
        [slix.repl defs listeners])
  (:import (java.io File OutputStreamWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *startup-script* 'replrc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-startup-script-file-name
  []
  *startup-script*)

(defn get-startup-script-file
  []
  (get-slix-file 'repl (get-startup-script-file-name)))

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
        tgt (with-create-sn-get-dir
              (File. (get-sid-slix-dir (slix-sn repl-slix)) (str (get-startup-script-file-name) ".clj")))]
    (when-not (.exists tgt)
      (clojure.java.io/copy src tgt))
    (if (and (map? (slix-args)) ((get-default :slix :arguments :alt-open) (slix-args)))
      src
      tgt)))

(defn start-repl*
  [slix repl-slix con ins rrc]
    (binding [*in*  (clojure.lang.LineNumberingPushbackReader. (.getIn con))
              *out* (OutputStreamWriter. (.getOut con))
              *err* (.getOut con)
              *slix* repl-slix]
      (let [it (fn []
                 (in-ns ins)
                 (use '[sevenri config core debug event jvm log slix os ui utils]
                      '[slix.repl.core :only (repl clear-repl-content)])
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
                                    (put-slix-prop slix :repl-thread rt)
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
     (let [op *slix*
           fr (slix-frame)
           cn (.getComponent (.getContentPane fr) 0)
           tp (.getTextPane cn)
           sd (.getDocument cn)
           rc (setup-replrc repl-slix)
           in (let [opon (slix-sn repl-slix)]
                (if (= opon 'repl)
                  'user
                  (get-slix-fqns opon)))
           sr #(start-repl* op repl-slix cn in rc)]
       ;; Remember start-in namespace, replrc, and console.
       (put-slix-prop :inns in)
       (put-slix-prop :replrc rc)
       (put-slix-prop :console cn)
       ;; Add listeners to textpane.
       (add-listeners tp sd cn sr op)
       ;; Start repl.
       (sr))))

(defn saving-repl
  [event]
  (if (= (slix-name) "Repl")
    ;; save only 'Repl' and don't report any XMLEncoder errors.
    (do
      (when (is-event-info-save-on-close? event)
        (.suspend (get-slix-prop :repl-thread)))
      (save-dyna-listeners
       [[(slix-frame) [(listener-triplet Window)]]
        [(.getTextPane (get-slix-prop :console)) [(listener-triplet Caret)
                                                   (listener-triplet Key)]]])
      (create-event-response :sevenri.event/response-suppress-xml-encoder-errors))
    ;; Discard others.
    (event-response-donot-save)))

(defn saved-repl
  [event]
  (when (and (= (slix-name) "Repl")
             (is-event-info-save-on-close? event))
    (.resume (get-slix-prop :repl-thread))))

(defn end-repl
  [event]
  (.stop (get-slix-prop :repl-thread))
  (.closeIO (get-slix-prop :console)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear-repl-content
  []
  (when-let [console (get-slix-prop :console)]
    (.setText (.getTextPane console) "")))
