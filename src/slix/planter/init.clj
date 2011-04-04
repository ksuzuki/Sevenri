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

(ns slix.planter.init
  (:use [sevenri log slix]
        [slix.planter controller core defs io ui])
  (:import (java.awt Cursor)
           (javax.swing JOptionPane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project
  [slix]
  (if-let [pn (:project (or (slix-args slix) (load-state slix)))]
    (if-let [slx (is-project-used pn)]
      (if-let [pn (get-unused-project)]
        (set-project-name slix pn)
        (do
          (set-project-name slix nil)
          (invoke-later slx #(.toFront (slix-frame slx)))
          (close-slix slix)
          nil))
      (set-project-name slix pn))
    (if-let [pn (get-unused-project)]
      (set-project-name slix pn)
      (let [lpn (last (keys (get-project-name-config-map)))
            slx (is-project-used lpn)]
        (assert (is-slix? slx))
        (set-project-name slix nil)
        (invoke-later slx #(.toFront (slix-frame slx)))
        (close-slix slix)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn can-load-lein?
  []
  (if (lein-loaded?)
    true
    (try
      (require 'lancet :reload)
      true
      (catch Exception e
        false))))

(defn load-lein
  []
  (when-not (lein-loaded?)
    #_(lg "planter: setup-planter: load-lein: loading lein")
    (lein-loaded? true)
    (doseq [l '[lancet lancet.core leiningen.core]]
      (require l :reload))
    ;; For some reasons, if these libs (and org.apache modules used by them)
    ;; were not preloaded, ClassCastException would occur when binding a fresh
    ;; ant-project to leiningen.core/ant-project and then calling
    ;; leiningen.core/-main.
    (doseq [l '[compile deps install test pom]]
      (require (symbol (str "leiningen." l))))))

(defn setup-planter
  "Lein is ready to use. Setup a lein agent. Also, if build-and-run is
   requested, do that and then close. Otherwise, register the fn in case
   it's requested."
  [slix]
  (let [f (slix-frame slix)
        c (.getCursor f)]
    (.setCursor f Cursor/WAIT_CURSOR)
    (load-lein)
    (create-lein-agent slix)
    (when-let [pn (get-project slix)]
      (invoke-later slix #(init-ui slix pn)))
    (.setCursor f c)
    ;;
    (if-let [bprm (*build-project-and-run* (slix-args slix))]
      (do-build-project-and-run slix (ui-controls slix) set-ui-wait bprm true)
      (add-do-build-project-and-run-to-xref slix (ui-controls slix) set-ui-wait))))

(defn is-planter-project-ready
  "-1 when failed, 0 when canceled, 1 when ready."
  [slix]
  (let [ao? (alt-open-slix? slix)
        frm (slix-frame slix)
        cur (.getCursor frm)
        msg (if (and (lein-loaded?) (not ao?))
              ;; lein-loaded but Planter project isn't ready, so...
              (str "Plater project may be deleted accidentally.\n"
                   "Please cancel and try rebuilding it by alt-opening Planter.")
              (str "Planter project is being built. Please wait for a while.\n"
                   "If this takes too long, you can cancel and try again."))
        ttl "Loading Planter Project"
        jop (JOptionPane. msg JOptionPane/WARNING_MESSAGE)
        dlg (.createDialog jop frm ttl)
        wtc (future (when ao?
                      (build-project? 'slix.planter))
                    (loop [pb? (is-project-built? 'slix.planter)]
                      (if pb?
                        (.hide dlg)
                        (do
                          (Thread/sleep 500)
                          (recur (is-project-built? 'slix.planter))))))]
    ;;
    (.setCursor frm Cursor/WAIT_CURSOR)
    (.setOptions jop (into-array ["Cancel"]))
    (when-not (future-done? wtc)
      (.show dlg))
    (.setCursor frm cur)
    ;;
    (when-not (future-done? wtc)
      (future-cancel wtc))
    (if (= (.getValue jop) "Cancel")
      0
      (if (is-project-built? 'slix.planter)
        1
        -1))))

(defn init-planter
  []
  (if (and (is-project-built? 'slix.planter) (can-load-lein?))
    ;; Planter project is ready and classpath to it is set.
    (setup-planter *slix*)
    ;;
    (if (and (not (lein-loaded?)) (< 1 (count (get-slixes 'planter))))
      ;; Open no more Planter because the very first Planter must be busy
      ;; preparing Planter project.
      (close-slix *slix*)
      ;;
      (let [rval (is-planter-project-ready *slix*)]
        (if (zero? rval) ;; cancelled
          (close-slix *slix*)
          (let [frm (slix-frame)
                msg (if (pos? rval)
                      "Planter project is ready.\n Please restart Planter."
                      "Planter project is not ready.\nTry rebuilding it by alt-opening Planter.")
                ttl (str "Planter Project Is " (when (neg? rval) "Not ") "Ready")
                ocr (.getCursor frm)]
            (.setCursor frm Cursor/WAIT_CURSOR)
            (JOptionPane/showMessageDialog frm msg ttl (if (pos? rval)
                                                         JOptionPane/INFORMATION_MESSAGE
                                                         JOptionPane/WARNING_MESSAGE))
            (.setCursor frm ocr)
            (close-slix *slix*)))))))
