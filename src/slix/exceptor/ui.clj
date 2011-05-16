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
  (:use [sevenri props slix ui])
  (:import (java.awt BorderLayout Toolkit)
           (javax.swing BorderFactory JPanel JTextArea)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-exceptor-frame
  []
  (let [fnm (:file (slix-args))
        lnm (:line (slix-args))
        xcp (:exception (slix-args))
        ms1 (format "%s: %s" (.getName fnm) lnm)
        ms2 (format "%s: %s" (.getName (class xcp)) (.getMessage xcp))
        txa (JTextArea. (str ms1 "\n" ms2))
        ;;
        sps (slix-props)
        frm (slix-frame)
        cpn (.getContentPane frm)
        sdm (.getScreenSize (Toolkit/getDefaultToolkit))
        [w h] (read-prop sps 'frame.size)]
    (doto txa
      (.setEditable false)
      (.setLineWrap true)
      (.setFont (create-font sps))
      (.setForeground (create-color (get-prop sps 'foreground.color)))
      (.setBackground (create-color (get-prop sps 'background.color)))
      (.setBorder (BorderFactory/createLineBorder (create-color (get-prop sps 'border.color))
                                                  (read-prop sps 'border.size))))
    (add-default-key-listener txa)
    (.add cpn txa)
    ;;
    (doto frm
      (.pack)
      (.setSize w h)
      (.setLocation (- (.getWidth sdm) w)
                    (- (.getHeight sdm) h)))))
