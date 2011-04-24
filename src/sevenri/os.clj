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

(ns ^{:doc "Sevenri interface library to OS depedent features"}
  sevenri.os
  (:use [sevenri config log]
        [sevenri.defs :only (*ok-to-quit-fn*)])
  (:import (java.io File)
           (javax.swing ImageIcon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-os-name
  []
  (System/getProperty "os.name"))

(defn is-mac?
  []
  (<= 0 (.indexOf (get-os-name) "Mac OS X")))

(defn is-win?
  []
  (<= 0 (.indexOf (get-os-name) "Windows")))

(defn is-linux?
  []
  (<= 0 (.indexOf (get-os-name) "Linux")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -set-mac-dock-icon
  []
  (let [idr (reduce (fn [d p] (File. d (str (get-config p))))
                    (get-user-path)
                    ['src.dir-name
                     'src.resources.dir-name
                     'src.resources.images.dir-name
                     'src.resources.images.icons.dir-name])
        icf (File. idr (get-config 'src.resources.images.icons.sevenri-icon-file-name))]
    (when (.exists icf)
      (let [ic (ImageIcon. (str icf))
            app (com.apple.eawt.Application/getApplication)]
        (.setDockIconImage app (.getImage ic))))))
    
(defn- -init-mac?
  []
  (future (-set-mac-dock-icon))
  #_(System/setProperty "apple.awt.graphics.UseQuartz" "true")
  (System/setProperty "apple.laf.useScreenMenuBar" "true")
  (let [app (com.apple.eawt.Application/getApplication)]
    (.removeAboutMenuItem app)
    (if *ok-to-quit-fn*
      (.addApplicationListener app (proxy [com.apple.eawt.ApplicationAdapter] []
                                     (handleQuit [e]
                                                 (.setHandled e (*ok-to-quit-fn*)))))
      (log-severe "-init-mac?: the quit handler is undefined")))
  true)

;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-ignorable-file-names-os
  []
  (cond
   (is-mac?) #{".DS_Store"}
   :else nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn startup-os?
  []
  (cond
   (is-mac?) (-ensure-processes
              -init-mac?)
   :else false))

(defn shutdown-os?
  []
  true)
