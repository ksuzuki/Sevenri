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

(ns oplix.documenter.init
  (:use [openar config core log oplix ui]
        [oplix.documenter io keymap listeners mddb md2html ui]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn initialize
  []
  (update-mddb)
  (add-keymap-to-editor (oplix-frame))
  (add-listeners (oplix-frame))
  (let [frame (oplix-frame)
        clist (get-control frame :category-list)
        cacts (get-control frame :category-actions)
        sectx (get-control frame :section-text)]
    (.setText sectx "")
    (doseq [c [clist cacts]]
      (.setEnabled c true))
    (doto clist
      (set-list-items (get-categories))
      (.requestFocusInWindow))
    ;;
    (when-let [stc (load-last-section-title-category *oplix*)]
      (let [tlist (get-control frame :title-list)
            slist (get-control frame :section-list)]
        (cond
         (and (= (count stc) 3) (get-src-section (first stc) (second stc) (last stc)))
           (invoke-later
            #(do
               (.setSelectedItem clist (last stc))
               (.setSelectedItem tlist (second stc))
               (.setSelectedItem slist (first stc))
               (doto sectx
                 (.setCaretPosition 0)
                 (.requestFocusInWindow))))
         (and (= (count stc) 2) (get-title (first stc) (second stc)))
           (invoke-later
            #(do
               (.setSelectedItem clist (second stc))
               (.setSelectedItem tlist (first stc))))
         (and (= (count stc) 1) (get-category (first stc)))
           (invoke-later #(.setSelectedItem clist (first stc))))))))

(defn save-current-work
  ([]
     (save-current-work (oplix-frame)))
  ([frm]
     (let [stx (get-control frm :section-text)
           stc (or (get-current-section-title-category frm)
                   (get-current-title-category frm)
                   [(get-current-category frm)])]
       (write-section stx)
       (save-current-section-title-category *oplix* stc))))
