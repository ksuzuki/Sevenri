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
  (:use [sevenri config defs log]
        [sevenri.defs :only (*ok-to-quit-fn*)])
  (:import (java.io File)
           (javax.swing ImageIcon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-mac-dock-icon
  []
  (let [fp (File. (get-resources-dir (get-default :src :resources :images :dir-name)
                                     (get-default :src :resources :images :icons :dir-name))
                  (str (get-default :src :resources :images :icons :sevenri-icon-file-name)))]
    (when (.exists fp)
      (let [ic (ImageIcon. (str fp))
            app (com.apple.eawt.Application/getApplication)]
        (.setDockIconImage app (.getImage ic))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-mac?
  []
  (= (System/getProperty "os.name") "Mac OS X"))

(defn- -init-mac
  []
  (set-mac-dock-icon)
  (System/setProperty "apple.laf.useScreenMenuBar" "true")
  (let [app (com.apple.eawt.Application/getApplication)]
    (.removeAboutMenuItem app)
    (if *ok-to-quit-fn*
      (.addApplicationListener app (proxy [com.apple.eawt.ApplicationAdapter] []
                                     (handleQuit [e]
                                                 (.setHandled e (*ok-to-quit-fn*)))))
      (log-severe "-init-mac: the quit handler is undefined")))
  true)

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
;;;; startup/shutdown

(defn startup-os?
  []
  (and true
       (cond
        (is-mac?) (-init-mac)
        :else true)))

(defn shutdown-os?
  []
  true)
