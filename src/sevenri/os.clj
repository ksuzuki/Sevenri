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

(ns sevenri.os
  "Sevenri interface lib to access OS depedent features"
  (:use [sevenri config defs log props])
  (:import (java.io File)
           (javax.swing ImageIcon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-os-name
  []
  (System/getProperty "os.name"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mac

(defn is-mac?
  []
  (<= 0 (.indexOf (get-os-name) "Mac OS X")))

(defn- -remove-mac-application-listener
  "Do this before saving a slix frame or addWindowListner call with
   AquaRootPaneUI instance will be left in the serialized form."
  [frame]
  (let [wls (.getWindowListeners frame)]
    (loop [wls (seq wls) removed []]
      (if (seq wls)
        (recur (rest wls) (if (= com.apple.laf.AquaRootPaneUI (class (first wls)))
                            (let [wl (first wls)
                                  removed (conj removed wl)]
                              (.removeWindowListener frame wl)
                              removed)
                            removed))
        removed))))

(defn- -add-mac-application-listener
  [frame windowListeners]
  (doseq [wl windowListeners]
    (.addWindowListener frame wl)))

(defn add-mac-about-handler
  [handler]
  (let [app (com.apple.eawt.Application/getApplication)]
    (.addApplicationListener app (proxy [com.apple.eawt.ApplicationAdapter] []
                                   (handleAbout [e]
                                     (when (fn? handler)
                                       (handler)
                                       (.setHandled e true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Windows

(defn is-win?
  []
  (<= 0 (.indexOf (get-os-name) "Windows")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Linux

(defn is-linux?
  []
  (<= 0 (.indexOf (get-os-name) "Linux")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn presave-slix-frame-os
  [frame]
  (cond
   (is-mac?) (-remove-mac-application-listener frame)
   :else nil))

(defn postsave-slix-frame-os
  [frame presave-slix-os-value]
  (cond
   (is-mac?) (-add-mac-application-listener frame presave-slix-os-value)
   :else nil))

;;;;

(defn get-ignorable-file-names-os
  []
  (cond
   (is-mac?) (read-prop (get-props) 'sevenri.platform.mac.ignorable-file-names)
   :else nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

;;;; Mac

(defn- -set-mac-dock-icon
  [app]
  (let [idr (reduce (fn [d p] (File. d (str (get-config p))))
                    (get-user-dir)
                    ['src.dir
                     'src.resources.dir
                     'src.resources.images.dir
                     'src.resources.images.icons.dir])
        icf (File. idr (get-config 'src.resources.images.icons.sevenri-icon-file-name))]
    (when (.exists icf)
      (let [ic (ImageIcon. (str icf))]
        (.setDockIconImage app (.getImage ic))))))
    
(defn- -init-mac?
  []
  (doseq [key ["apple.awt.graphics.UseQuartz"
               "apple.laf.useScreenMenuBar"]]
    (let [val (if (= (str (get-prop (get-props) key)) "true") "true" "false")]
      (System/setProperty key val)))
  ;;
  (let [app (com.apple.eawt.Application/getApplication)]
    (future (-set-mac-dock-icon app))
    (if (fn? *ok-to-quit-fn*)
      (do
        (.addApplicationListener app (proxy [com.apple.eawt.ApplicationAdapter] []
                                       (handleQuit [e]
                                         (.setHandled e (*ok-to-quit-fn*)))))
        true)
      (do
        (log-severe "-init-mac?: the quit handler is undefined")
        false))))

;;;; Windows

;;;; Linux

;;;;

(defn startup-os?
  []
  (cond
   (is-mac?) (apply while-each-true?
                    (do-each-after* print-fn-name*
                     -init-mac?))
   :else false))

(defn shutdown-os?
  []
  (cond
   (is-mac?) (apply while-each-true?
                    nil)
   :else false))
