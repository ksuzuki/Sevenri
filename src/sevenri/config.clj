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

(ns sevenri.config
  (:use [sevenri defs utils])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *making-dir* false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *sevenri-defaults*
 {:doc {:dir-name 'doc
        :apidoc-urls-file-name 'apidoc_urls.clj}

  :lib {:dir-name 'lib}

  ;; sid := system instance directory (.sevenri)
  :sid {:dir-name '.sevenri

        :classes {:dir-name 'classes
                  :slix {:dir-name 'slix}
                  :sevenri {:dir-name 'sevenri}}

        :log {:dir-name 'log
              :logger-name 'Sevenri-logger
              :logger-header "sevenri:"
              :file-body-name 'sevenri
              :file-count 5}

        :sevenri {:dir-name 'sevenri
                 :exception {:dir-name 'exception
                             :listeners 'exception_listeners.clj}
                 :lock-file-name 'lock}

        :slix {:dir-name 'slix
                :save {:dir-name '_save_
                       :frame-file-name 'frame.xml
                       :state-file-name 'state.clj}
                :startup {:file-name '_startup_.clj}}

        :temp {:dir-name 'temp}

        :trash {:dir-name 'trash
                :sid {:dir-name 'sid}}}

  :src {:dir-name 'src
        
        :library {:dir-name 'library
                  :slix {:dir-name 'slix}
                  :user {:dir-name 'user
                         :scratch-file-name 'scratch.clj}}
        
        :project {:dir-name 'project
                  :protocol-file-name 'protocol.clj}
        
        :resources {:dir-name 'resources
                    :images {:dir-name 'images
                             :icons {:dir-name 'icons
                                     :sevenri-icon-file-name 'Sevenri-icon.png}}
                    :logger {:dir-name 'logger
                             :configuration-file 'logger.conf
                             :dtd-file 'logger.dtd}}
        
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

  :os {:ignorable-file-names #{".DS_Store"}}

  :slix {:arguments {:alt-open :alt-open}}

  :startup {:aot-compile-list '(sevenri)}

  ;; tln := sevenri/slix top level namespaces
  :tln {:library 'library
        :project 'project
        :resources 'resources
        :sevenri 'sevenri
        :slix 'slix}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-default
  [& ks]
  (get-in *sevenri-defaults* ks))

(defn get-dir
  "Return a directory File object based on root and optional postfixes.
   ns2path conversion is applied to each postfix but not to root if it's
   a File object already. Also, if the directory doesn't exist and
   *making-dir* is true, the directory will be created before returning."
  [root & pfxs]
  (let [rdr (if (instance? File root)
              root
              (File. (ns2path root)))
        dir (reduce (fn [d p] (File. d (ns2path p))) rdr pfxs)]
    (when (and *making-dir* (not (.exists dir)))
      (.mkdirs dir))
    dir))

(defmacro with-making-dir
  "Set up a context to let get-dir make directory if necessary."
  [& body]
  `(binding [*making-dir* true]
     ~@body))

(defmacro def-get-dir
  [name root]
  (let [gnd (symbol (str "get-" name "-dir"))]
    `(defn ~gnd
       [& ~'pfxs]
       (apply get-dir ~root ~'pfxs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-get-dir doc (get-dir (get-user-dir) (get-default :doc :dir-name)))

(def-get-dir lib (get-dir (get-user-dir) (get-default :lib :dir-name)))

(def-get-dir src (get-dir (get-user-dir) (get-default :src :dir-name)))

(def-get-dir src-library (get-dir (get-src-dir) (get-default :src :library :dir-name)))
(defmacro get-library-dir [& pfxs] `(get-src-library-dir ~@pfxs))

(def-get-dir src-project (get-dir (get-src-dir) (get-default :src :project :dir-name)))
(defmacro get-project-dir [& pfxs] `(get-src-project-dir ~@pfxs))

(def-get-dir src-resources (get-dir (get-src-dir) (get-default :src :resources :dir-name)))
(defmacro get-resources-dir [& pfxs] `(get-src-resources-dir ~@pfxs))

(def-get-dir temp (get-dir (get-user-dir) (get-default :temp :dir-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sid-root-dir
  ([]
     (or *sid-dir* (get-sid-root-dir (get-default :sid :dir-name) 0)))
  ([dname n]
     (let [dir (File. (get-user-dir) (if (pos? n) (str dname n) (str dname)))]
       (when-not (.exists dir)
         (.mkdir dir))
       (if (and (.isDirectory dir) (.canWrite dir))
         (do
           (reset-sid-dir dir)
           *sid-dir*)
         (get-sid-root-dir dname (inc n))))))

(def-get-dir sid (get-sid-root-dir))

(def-get-dir sid-classes (get-sid-dir (get-default :sid :classes :dir-name)))

(def-get-dir sid-temp (get-sid-dir (get-default :sid :temp :dir-name)))

(def-get-dir sid-trash (get-sid-dir (get-default :sid :trash :dir-name)))
