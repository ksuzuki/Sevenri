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

(ns openar.config
  (:use [openar defs utils])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *create-on-get-dir* false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *openar-defaults*
 {:src {:dir-name 'src
        :library {:dir-name 'library
                  :oplix {:dir-name 'oplix}
                  :user {:dir-name 'user
                         :scratch-file-name 'scratch.clj}}
        :openar {:dir-name 'openar
                 :listeners {:dir-name 'listeners
                             :aot 'openar.listeners.aot
                             :awtehandler 'openar.listeners.awtehandler
                             :defwinlistener 'openar.listeners.defwinlistener
                             :defkeylistener 'openar.listeners.defkeylistener}}
        :oplix {:dir-name 'oplix
                :jvm {:dir-name 'jvm}}
        :resources {:dir-name 'resources
                    :images {:dir-name 'images
                             :icons {:dir-name 'icons
                                     :openar-icon-file-name 'openar-icon.png}}
                    :logger {:dir-name 'logger
                             :configuration-file 'logger.conf
                             :dtd-file 'logger.dtd}}}

  ;; on - openar/oplix top level namespaces
  :on {:library 'library
       :openar 'openar
       :oplix 'oplix
       :resources 'resources}

  ;; dop - dot openar (.openar)
  :dop {:dir-name '.openar

        :classes {:dir-name 'classes
                  :oplix {:dir-name 'oplix}
                  :openar {:dir-name 'openar}}

        :log {:dir-name 'log
              :logger-name 'Openar-logger
              :logger-header "openar:"
              :file-body-name 'openar
              :file-count 5}

        :openar {:dir-name 'openar
                 :exception {:dir-name 'exception
                             :listeners 'exception_listeners.clj}
                 :lock-file-name 'lock}

        :oplix {:dir-name 'oplix
                :save {:dir-name '_save_
                       :frame-file-name 'frame.xml
                       :state-file-name 'state.clj}
                :startup {:file-name '_startup_.clj}}

        :temp {:dir-name 'temp}

        :trash {:dir-name 'trash
                :dop {:dir-name 'dop}}}

  :doc {:dir-name 'doc
        :apidoc-urls-file-name 'apidoc_urls.clj}

  :lib {:dir-name 'lib}
  
  :temp {:dir-name 'temp}
  
  ;;;;
  
  :debug {:env-name "OPENAR_RUN_SWANK_REPL"
          :swank-encoding "utf-8-unix"
          :swank-port 4009}
  
  :startup {:aot-compile-list '(openar)}

  :frame {:width 320
          :height 200}

  :oplix {:arguments {:alt-open :alt-open}}
  
  :os {:ignorable-file-names #{".DS_Store"}}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-default
  [& ks]
  (get-in *openar-defaults* ks))

(defn get-dir
  [root & pfxs]
  (let [dir (if pfxs
              (reduce (fn [d p] (File. d (nssym2path p))) root pfxs)
              (File. (str root)))]
    (when (and *create-on-get-dir* (not (.exists dir)))
      (.mkdirs dir))
    dir))

(defmacro with-create-on-get-dir
  [& body]
  `(binding [*create-on-get-dir* true]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-doc-dir
  [& pfxs]
  (apply get-dir (File. (get-user-dir) (str (get-default :doc :dir-name))) pfxs))
  
(defn get-lib-dir
  [& pfxs]
  (apply get-dir (File. (get-user-dir) (str (get-default :lib :dir-name))) pfxs))

(defn get-src-dir
  [& pfxs]
  (apply get-dir (File. (get-user-dir) (str (get-default :src :dir-name))) pfxs))

(defn get-src-library-dir
  [& pfxs]
  (apply get-dir (File. (get-src-dir) (str (get-default :src :library :dir-name))) pfxs))

(defmacro get-library-dir
  [& pfxs]
  `(get-src-library-dir ~@pfxs))

(defn get-src-resources-dir
  [& pfxs]
  (apply get-dir (File. (get-src-dir) (str (get-default :src :resources :dir-name))) pfxs))

(defmacro get-resources-dir
  [& pfxs]
  `(get-src-resources-dir ~@pfxs))

(defn get-temp-dir
  [& pfxs]
  (apply get-dir (File. (get-user-dir) (str (get-default :temp :dir-name))) pfxs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-dop-root-dir
  ([]
     (or *dop-dir* (get-dop-root-dir (get-default :dop :dir-name) 0)))
  ([dname n]
     (let [dir (File. (get-user-dir) (if (pos? n) (str dname n) (str dname)))]
       (when-not (.exists dir)
         (.mkdir dir))
       (if (and (.isDirectory dir) (.canWrite dir))
         (do
           (reset-dop-dir dir)
           *dop-dir*)
         (get-dop-root-dir dname (inc n))))))

(defn get-dop-dir
  [& pfxs]
  (apply get-dir (get-dop-root-dir) pfxs))

(defn get-dop-classes-dir
  [& pfxs]
  (apply get-dir (get-dop-dir (get-default :dop :classes :dir-name)) pfxs))

(defn get-dop-temp-dir
  [& pfxs]
  (apply get-dir (get-dop-dir (get-default :dop :temp :dir-name)) pfxs))

(defn get-dop-trash-dir
  [& pfxs]
  (with-create-on-get-dir
    (apply get-dir (get-dop-dir (get-default :dop :trash :dir-name)) pfxs)))
