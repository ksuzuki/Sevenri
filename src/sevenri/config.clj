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

(ns ^{:doc "Sevenri system configuration library"}
  sevenri.config
  (:use sevenri.defs)
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *sevenri-config*
 {:doc {:dir-name 'doc
        :apidoc-urls-file-name "apidoc_urls.clj"}

  :lib {:dir-name 'lib}

  ;; sid := system instance directory (.sevenri)
  :sid {:dir-name '!sevenri :dir-name-str ".sevenri"

        :classes {:dir-name 'classes
                  :slix {:dir-name 'slix}
                  :sevenri {:dir-name 'sevenri}}

        :log {:dir-name 'log
              :logger-name 'Sevenri-logger
              :logger-header "sevenri:"
              :file-name "sevenri"
              :file-count 5}

        :sevenri {:dir-name 'sevenri
                 :exception {:dir-name 'exception
                             :listeners 'exception_listeners}
                 :lock-file-name "lock"}

        :slix {:dir-name 'slix
                :save {:dir-name '-save-
                       :frame-file-name "frame.xml"
                       :state-file-name "state.clj"}
                :startup {:file-name "_startup_.clj"}}

        :temp {:dir-name 'temp}

        :trash {:dir-name 'trash
                :sid {:dir-name 'sid}}}

  :src {:dir-name 'src
        
        :library {:dir-name 'library
                  :slix {:dir-name 'slix}
                  :user {:dir-name 'user
                         :scratch-file-name "scratch.clj"}}
        
        :project {:dir-name 'project
                  :protocol-file-name "protocol.clj"}
        
        :resources {:dir-name 'resources
                    :images {:dir-name 'images
                             :icons {:dir-name 'icons
                                     :sevenri-icon-file-name "Sevenri-icon.png"}}
                    :logger {:dir-name 'logger
                             :config-file-name "logger.conf"
                             :dtd-file-name "logger.dtd"}}
        
        :sevenri {:dir-name 'sevenri
                 :listeners {:dir-name 'listeners
                             :aot 'sevenri.listeners.aot
                             :awtehandler 'sevenri.listeners.awtehandler
                             :defkeylistener 'sevenri.listeners.defkeylistener
                             :defwinlistener 'sevenri.listeners.defwinlistener
                             :evtdelegator 'sevenri.listeners.evtdelegator}}
        
        :slix {:dir-name 'slix
                :jvm {:dir-name 'jvm}}}

  :temp {:dir-name 'temp}
  
  ;;;;
  
  :debug {:env-name "SEVENRI_RUN_SWANK_REPL"
          :swank-encoding "utf-8-unix"
          :swank-port 4009}
  
  :frame {:width 320
          :height 200}

  :slix {:arguments {:alt-open :alt-open}}

  :top-level-ns #{'library 'project 'resources 'sevenri 'slix}})

;;;;

(defn get-config
  "Sevenri configuration information access fn. Return a value of a target
   configuration keyword which is expressed in a format of each interim
   keyword connected by period."
  [key]
  (let [keys (map #(keyword %) (.split (str key) "\\."))]
    (get-in *sevenri-config* keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Base directory path accessors

(defn get-user-home-path
  []
  (System/getProperty "user.home"))

(defn get-class-path
  []
  (System/getProperty "java.class.path"))

(defn get-user-path
  []
  (System/getProperty "user.dir"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sid (Sevenri instance directory) structure
;; sid is created in the .sevenri directory at the user's home directory.
;; Each Sevenri instance name is given by the system property
;; 'sevenri.sid.name' or "Sevenri" by default.
;;
;; /user.home/.sevenri/
;;   Sevenri/
;;     sevenri/
;;   Sevenri-X/
;;     sevenri/
;;   Sevenri-Y/
;;     sevenri/

(defn get-sid-name
  "Return the current Sevenri instance directory name. Preset to 'Sevenri'
   by default and reset to the one given at the startup time."
  []
  *sid-name*)

(defn create-sid*
  "Create the sid root directory at the user's home directory, then create a
   Sevenri instance directory and the 'sevenri' sub directory."
  ([]
     (let [sid-name (if-let [sn (System/getProperty "sevenri.sid.name")]
                      sn
                      (get-sid-name))]
       (create-sid* sid-name)))
  ([sid-name]
     (let [sid-root (File. (get-user-home-path) (get-config 'sid.dir-name-str))
           sid-path (File. sid-root sid-name)
           sid-sevenri-dir (File. sid-path (str (get-config 'sid.sevenri.dir-name)))]
       (doseq [path [sid-root sid-path sid-sevenri-dir]]
         (when-not (.exists path)
           (when-not (.mkdir path)
             (throw (RuntimeException. (str "create-sid: mkdir failed:" path))))))
       (reset-sid-name sid-name)
       (reset-sid-path sid-path)
       *sid-path*)))
