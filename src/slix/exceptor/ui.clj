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

(ns slix.exceptor.ui
  (:use [sevenri slix ui]
        [slix.exceptor defs])
  (:import (java.awt BorderLayout Color Toolkit)
           (javax.swing BorderFactory JPanel JTextArea)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-ui
  []
  (let [fnm (:file (slix-args))
        lnm (:line (slix-args))
        xcp (:exception (slix-args))
        ms1 (format "%s: %s" (.getName fnm) lnm)
        ms2 (format "%s: %s" (.getName (class xcp)) (.getMessage xcp))
        txa (JTextArea. (str ms1 "\n" ms2))
        frm (slix-frame)
        cpn (.getContentPane frm)
        sdm (.getScreenSize (Toolkit/getDefaultToolkit))]
    (doto txa
      (.setEditable false)
      (.setLineWrap true)
      (.setFont (create-font "Times" 'BOLD 14))
      (.setForeground Color/yellow)
      (.setBackground Color/black)
      (.setBorder (BorderFactory/createLineBorder Color/black 6)))
    ;;
    (add-default-key-listener txa)
    (.add cpn txa)
    (doto frm
      (.pack)
      (.setSize *width* *height*)
      (.setLocation (- (.getWidth sdm) *width*)
                    (- (.getHeight sdm) *height*)))))
