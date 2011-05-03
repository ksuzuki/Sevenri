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
 {:doc {:dir 'doc
        :apidoc-urls-file-name "apidoc_urls.clj"}

  ;; dsr := 'dot Sevenri'
  :dsr {:dir '!sevenri
        :dir-name ".sevenri"
        :properties {:dir 'properties
                     :user-file-name "user.properties"}}

  :lib {:dir 'lib}

  ;; sid := system instance directory
  :sid {:dir 'Sevenri ;; default

        :classes {:dir 'classes
                  :slix {:dir 'slix}
                  :sevenri {:dir 'sevenri}}

        :log {:dir 'log
              :logger 'Sevenri-logger
              :logger-header "sevenri:"
              :file-name "sevenri"
              :file-count 5}

        :properties {:dir 'properties
                     :user-file-name "user.properties"
                     :persistent-file-name "persistent.properties"}

        :sevenri {:dir 'sevenri
                 :exception {:dir 'exception
                             :listeners-file-name "exception_listeners.clj"}
                 :lock-file-name "lock"}

        :slix {:dir 'slix
               :properties {:dir 'properties
                            :user-file-name "user.properties"}
               :save {:dir '-save-
                      :frame-file-name "frame.xml"
                      :state-file-name "state.clj"
                      :persistent-file-name "persistent.properties"}
               :startup {:file-name "_startup_.clj"}}

        :temp {:dir 'temp}

        :trash {:dir 'trash
                :sid {:dir 'sid}}}

  :src {:dir 'src
        
        :library {:dir 'library
                  :slix {:dir 'slix}
                  :user {:dir 'user
                         :scratch-file-name "scratch.clj"}}
        
        :project {:dir 'project}

        :properties {:dir 'properties
                     :default-file-name "default.properties"}
        
        :resources {:dir 'resources
                    :images {:dir 'images
                             :icons {:dir 'icons
                                     :sevenri-icon-file-name "Sevenri-icon.png"}}
                    :logger {:dir 'logger
                             :config-file-name "logger.conf"
                             :dtd-file-name "logger.dtd"}}
        
        :sevenri {:dir 'sevenri
                  :listeners {:dir 'listeners
                              :aot 'sevenri.listeners.aot
                              :awtehandler 'sevenri.listeners.awtehandler
                              :defkeylistener 'sevenri.listeners.defkeylistener
                              :defwinlistener 'sevenri.listeners.defwinlistener
                              :evtdelegator 'sevenri.listeners.evtdelegator}}
        
        :slix {:dir 'slix
               :jvm {:dir 'jvm}
               :properties {:dir 'properties
                            :default-file-name "default.properties"}}

        :top-level-ns #{'library 'project 'properties 'resources 'sevenri 'slix}}

  :temp {:dir 'temp}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-user-dir
  []
  *user-dir*)

(defn get-user-home
  []
  *user-home*)

(defn get-env
  [name]
  (System/getenv (str name)))

(defn get-system-properties
  ([]
     (get-system-properties ".*"))
  ([regex]
     (filter #(re-find (re-pattern (str regex)) (str %)) (System/getProperties))))

(defn get-config
  "Sevenri configuration information access fn. Return a value of a target
   configuration keyword which is expressed in a format of each interim
   keyword connected by period."
  [key]
  (let [keys (map #(keyword %) (.split (str key) "\\."))]
    (get-in *sevenri-config* keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dsr (dot Sevenri) and sid (Sevenri instance directory) structure
;; dsr is created at the user's home directory. sid, in turn, is created in
;; the dsr directory. Each sid has the 'sevenri' sub directory at least.
;;
;; /user.home/.sevenri/
;;   Sevenri/
;;     sevenri/
;;   Sevenri-X/
;;     sevenri/
;;   Sevenri-Y/
;;     sevenri/

(defn get-sid-name
  "Return the current Sevenri instance directory name."
  []
  (or *sid-name*
      (System/getProperty "sevenri.sid.name")
      (get-config 'sid.dir)))

(defn create-sid*
  "Create a Sevenri instance directory in the 'dot Sevenri' directory.
   Everything depends on the paths setup here, so this fn must be called
   at the very early stage of the system startup."
  ([]
     (create-sid* (get-sid-name)))
  ([sid-name]
     (let [dsr-path (File. (get-user-home) (get-config 'dsr.dir-name))
           sid-path (File. dsr-path sid-name)
           sid-sevenri-dir (File. sid-path (str (get-config 'sid.sevenri.dir)))]
       (doseq [path [dsr-path sid-path sid-sevenri-dir]]
         (when-not (.exists path)
           (when-not (.mkdir path)
             (throw (RuntimeException. (str "create-sid: mkdir failed:" path))))))
       (reset-dsr-path dsr-path)
       (reset-sid-name sid-name)
       (reset-sid-path sid-path)
       *sid-path*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown
;;
;; Fns and macros used at startup and shutdown.

(defmacro do-each-after*
  [prefn & fns]
  (let [pfns (map #(list 'fn '[] (list prefn %) (list %))
                  fns)]
    `(list ~@pfns)))

(defmacro print-fn-name*
  [f]
  `(sevenri.log/print-info (:name (meta ~f))))

(defn while-each-true?
  [& bfns]
  (if (seq bfns)
    (let [bfn (first bfns)]
      (if (bfn)
        (recur (next bfns))
        false))
    true))

(defn using-fn-prefix
  [fquns]
  (let [uns (if-let [m (re-matches #"^sevenri\.(.*)$" (str fquns))]
              (second m)
              fquns)]
    (str uns "-using-")))

(defmacro using-fns
  "uns is using rfns of rns. Expand to defns of uns-using-rfn-rns"
  [uns rns rfns]
  (let [ufp (using-fn-prefix uns)
        dfs (map #(list 'defn
                        (symbol (str ufp % \- rns))
                        '[& args]
                        (vector (list 'quote %) (list 'quote rns)))
                 rfns)]
    `(do ~@dfs)))
