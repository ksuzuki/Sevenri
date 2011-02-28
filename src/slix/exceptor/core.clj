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

(ns slix.exceptor.core
  (:use [sevenri config log slix utils]
        [slix.exceptor defs edb refs])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-launch
  ([#^Exception e file line]
     (do-launch e file line true))
  ([#^Exception e file line open-ced?]
     (add-to-edb e file line open-ced?)))

(defn launch-exceptor
  [#^Exception e fqsn file-name line-number]
  #_(lg "fqsn:" fqsn "file-name:" file-name "line-number:" line-number)
  (if (re-find (re-pattern (str (get-slix-fqns *ced*))) (str fqsn))
    ;; Cannot open ced when it's the cause of the exception.
    (do-launch e (File. file-name) line-number false)
    (let [sub-path (nssym2path fqsn)
          clj-file (File. (get-src-dir) (str sub-path ".clj"))]
      (if (and (= (.getName clj-file) file-name) (.exists clj-file))
        (do-launch e clj-file line-number)
        (let [clj-file (File. (get-src-dir) (str sub-path "/" file-name))]
          (if (.exists clj-file)
            (do-launch e clj-file line-number)
            (let [sp (seq (.split sub-path "/"))]
              (if (< 2 (count sp)) ;; e.g. slix/x/y
                (let [sub-path (apply str (butlast (interleave (butlast sp) (repeat "/"))))
                      clj-files (find-files '.clj (File. (get-src-dir) sub-path))
                      cand-clj-files (filter #(= file-name (.getName %)) clj-files)]
                  (if (seq cand-clj-files)
                    (let [f (first cand-clj-files)]
                      ;; FIX ME; what if multiple files with the same name found?
                      (do-launch e f line-number))
                    (do-launch e (File. file-name) line-number false)))
                (do-launch e (File. file-name) line-number false)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn register-exception-handler
  "(Re)register the exception listener."
  []
  (when-not (seq (filter #(= 'exceptor/handle-exception %) (keys (get-exception-listeners))))
    (register-exception-listener 'exceptor 'handle-exception)))
