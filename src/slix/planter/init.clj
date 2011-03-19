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
  (:import (java.awt Cursor)))

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

(defn init-ui
  [sym]
  (let [nmcnfgmp (get-project-name-config-map)
        controls (ui-controls)
        prjnames (:project-names controls)
        itmlstrs (seq (.getItemListeners prjnames))]
    (doseq [l itmlstrs]
      (.removeItemListener prjnames l))
    ;;
    (.putClientProperty prjnames *name-config-map* nmcnfgmp)
    (.removeAllItems prjnames)
    (doseq [nm (sort (keys nmcnfgmp))]
      (.addItem prjnames (str nm)))
    ;; Do this or the setSelectedItem call below won't work.
    (.setSelectedIndex prjnames -1)
    ;;
    (doseq [l itmlstrs]
      (.addItemListener prjnames l))
    ;;
    (.setSelectedItem prjnames (str sym))
    (.setDividerLocation (:splitter controls) 0.3)
    (set-title sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-lein
  []
  (when-not (lein-loaded)
    #_(lg "planter: setup-planter: load-lein: loading lein")
    (lein-loaded true)
    (doseq [l '[lancet lancet.core leiningen.core]]
      (require l :reload))
    ;; For some reasons, if these libs (and org.apache modules used by them)
    ;; were not preloaded, ClassCastException would occur when binding a fresh
    ;; ant-project to leiningen.core/ant-project and then calling
    ;; leiningen.core/-main.
    (doseq [l '[compile deps install test pom]]
      (require (symbol (str "leiningen." l))))))

(defn setup-planter
  [slix]
  (let [f (slix-frame slix)
        c (.getCursor f)]
    (.setCursor f Cursor/WAIT_CURSOR)
    ;;
    (load-lein)
    (when-let [pn (get-project slix)]
      (create-lein-agent slix)
      (invoke-later slix #(init-ui pn)))
    ;;
    (.setCursor f c)))

(defn init-planter
  []
  (if (is-project-built? 'slix.planter)
    (setup-planter *slix*)
    (let [slx *slix*
          frm (slix-frame)
          ao? (alt-open-slix?)]
      (future
        (let [oc (.getCursor frm)]
          (.setCursor frm Cursor/WAIT_CURSOR)
          ;;
          (when ao?
            (let [bp? (build-project? 'slix.planter)
                  msg (if bp? "succeeded" "failed")]
              (when bp?
                (setup-planter slx))
              (lg "slix.planter: init: building the planter project" msg)))
          (let [b? (is-project-built? 'slix.planter)]
            (when-not b?
              (log-warning "slix.planter: init: planter project isn't ready.")
              (Thread/sleep (* 1000 3)))
            ;;
            (.setCursor frm oc)
            (when-not b?
              (close-slix slx))))))))
