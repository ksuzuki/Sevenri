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

(ns oplix.incantea.core
  (:require clojure.set)
  (:use [openar config core log oplix utils]
        [oplix.incantea defs])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-incanter
  []
  (when-not (System/getProperty *incanter-home*)
    (System/setProperty *incanter-home* (str (get-library-oplix-dir 'incantea)))))

(defmacro use-incanter
  "Refer to Incanter libs core, charts, datasets, processing, and stats with
   no argument. Refer to other Incanter libs additionally with lib name(s).
   The names donot need to be fully qualified."
  [& modules]
  (let [core# '[incanter.core :rename {copy _copy
                                       quit _quit
                                       view _view}]
        charts# '[incanter.charts :rename {dynamic-xy-plot _dynamic-xy-plot
                                           dynamic-scatter-plot _dynamic-scatter-plot
                                           slider _slider
                                           sliders* _sliders*
                                           sliders _sliders
                                           view _view}]
        datasets# '[incanter.datasets]
        processing# '[incanter.processing :rename {view _view}]
        stats# '[incanter.stats]
        others# (when (seq modules)
                  (let [modset (apply hash-set (map symbol modules))
                        stdset #{'core 'charts 'datasets 'processing 'stats}]
                  (list (apply vector 'incanter (clojure.set/difference modset stdset)))))
        all# (map #(list 'quote %)
                  (concat (list core# charts# datasets# processing# stats#) others#))]
    `(use ~@all#)))

(defn get-using-incanter
  []
  (reduce (fn [s v]
            (conj s (second (re-matches #"^#'incanter\.(.*)/.*$" (str v)))))
          #{}
          (filter #(re-matches #"^#'incanter\..*" (str %)) (vals (ns-refers *ns*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-sample-datasets
  []
  (let [sdf (get-library-oplix-file 'incantea *sample-datasets*)]
    (when (.exists sdf)
      (try
        (let [sd (read-string (slurp sdf :encoding "UTF-8"))]
          (when (and (map? sd) (string? (:url sd)) (vector? (:names sd)))
            sd))
        (catch Exception e
          (log-warning "incantea: read-sample-datasets failed")
          nil)))))

(defn download-datasets
  "Download datasesets. datasets is a map of these key-values:
     :url - URL string
     :names - vector of dataset names
     :update? - optional boolean
   By default a map returned from read-sample-datasets is used as datasets.
   No download occurs for dataset when it exists. Set true to :update? to
   update existing datasets."
  ([]
     (when-let [sd (read-sample-datasets)]
       (download-datasets sd)))
  ([datasets]
     (when (map? datasets)
       (let [ds (merge (read-sample-datasets) datasets)]
         (when (and (string? (:url ds)) (vector? (:names ds)))
           (with-create-on-get-dir
             (let [data-dir (get-library-oplix-dir 'incantea 'data)]
               (future
                 (doseq [name (:names ds)]
                   (let [url (str (:url ds) name)
                         out (File. data-dir (str name))]
                     (when (or (not (.exists out)) (true? (:update? ds)))
                       (try
                         (with-open [istream (.openStream (java.net.URL. url))
                                     ostream (java.io.FileOutputStream. out)]
                           (loop [b (.read istream)]
                             (when-not (neg? b)
                               (.write ostream b)
                               (recur (.read istream)))))
                         (catch Exception e
                           (log-warning "incantea: download-datasets failed: " url))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn norm-spell-name
  [spell-name]
  (-> (str spell-name)
      (.replace \space \-)
      (.replace \/ \.)))

(defn get-spell-name-from-*ns*
  []
  (let [rp (re-pattern (str "^" *spell-ns* "\\.(.*)"))
        rm (re-matches rp (str (ns-name *ns*)))]
    (second rm)))

(defn get-spell-ns
  ([]
     (when-let [sn (get-spell-name-from-*ns*)]
       (get-spell-ns sn)))
  ([spell-name]
     (let [spell (norm-spell-name (str spell-name))]
       [(symbol (str *spell-ns* \. spell))
        (get-library-oplix-fqns 'incantea.spells spell)])))

(defn get-spells-dir
  []
  (with-create-on-get-dir
   (get-library-oplix-dir 'incantea.spells)))

(defn get-spell-files
  []
  (find-files '.clj (get-spells-dir)))

(defn get-spell-file
  [spell-name]
  (let [spell (norm-spell-name spell-name)]
    (File. (get-spells-dir) (str (nssym2path spell) ".clj"))))

(defn get-spell-name-from-spell-file
  [spell-file]
  (let [rep (re-pattern (str "^" (get-spells-dir) "/" "(.*)\\.clj$"))]
    (path2nssym (second (re-matches rep (str spell-file))))))

(defn get-spell-names
  []
  (sort (map #(str (get-spell-name-from-spell-file %)) (get-spell-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-new-spell-name
  []
  (let [sns (get-spell-names)]
    (loop [num 1
           nsn *new-spell-name*]
      (if (some #(= nsn %) sns)
        (recur (inc num) (str *new-spell-name* num))
        nsn))))

(defn generate-new-spell
  [spell-name]
  (let [[_ spell-lib-ns] (get-spell-ns spell-name)]
    (format (str "(ns %s\n"
                 "  (:use [oplix.incantea core ui]))\n"
                 "\n"
                 ";; Using core, charts, datasets, processing, and stats by default.\n"
                 "(use-incanter)\n"
                 "\n"
                 "(defn incant\n"
                 "  [])\n"
                 "\n"
                 ";; (Re)load the above in the spell.%s namespace. This should be the last line.\n"
                 "(create-spell-ns '%s)\n")
                 spell-lib-ns spell-name spell-name)))

(defn create-new-spell-file
  [spell-name]
  (let [file (get-spell-file spell-name)
        content (generate-new-spell spell-name)]
      (spit file content :encoding "UTF-8")
      file))

(defn load-spell
  ([]
     (when-let [sn (get-spell-name-from-*ns*)]
       (load-spell sn)))
  ([spell-name]
     (let [spell (norm-spell-name spell-name)
           [spell-ns lib-spell-ns] (get-spell-ns spell)
           sfile (get-spell-file spell)
           loaded? (if (.exists sfile)
                     (try
                       (load-file (str sfile))
                       true
                       (catch Exception e
                         (log-exception e lib-spell-ns)
                         false))
                     false)
           lib-ns-exists? (if (and loaded? (find-ns lib-spell-ns))
                            true
                            false)
           incant-defined? (if lib-ns-exists?
                             (let [v (ns-resolve lib-spell-ns 'incant)]
                               (if (and v (fn? (var-get v)))
                                 true
                                 false))
                             false)]
       {:spell-name spell
        :spell-file sfile
        :spell-ns spell-ns
        :lib-spell-ns lib-spell-ns
        :loaded? loaded?
        :lib-ns-exists? lib-ns-exists?
        :incant-defined? incant-defined?})))

(defn create-spell-ns
  "(Re)create namespace spell.spell-name with the references to the spell
   namespace spell-name, some Incanter libs, and clojure.core."
  ([spell-name]
     (let [[sn lsn] (get-spell-ns spell-name)]
       (create-spell-ns spell-name sn lsn)))
  ([spell-name spell-ns lib-spell-ns]
    (binding [*ns* *ns*]
      (in-ns spell-ns)
      (clojure.core/refer-clojure)
      (use '[oplix.incantea core ui])
      (use-incanter)
      (when (and lib-spell-ns (not (find-ns lib-spell-ns)))
        (load-spell spell-name))
      (when (and lib-spell-ns (find-ns lib-spell-ns))
        (use lib-spell-ns)))))

(defn in-spell-ns
  ([]
     (let [spell-name (str *spell-ns*)]
       (create-spell-ns spell-name *spell-ns* nil)
       (in-spell-ns spell-name *spell-ns*)))
  ([spell-name]
     (let [[spell-ns lib-spell-ns] (get-spell-ns spell-name)]
       (create-spell-ns spell-name spell-ns lib-spell-ns)
       (in-spell-ns spell-name spell-ns)))
  ([spell-name spell-ns]
     (let [replrc (get-dop-oplix-file 'incantea.replrc)]
       (in-ns spell-ns)
       (use '[openar config core oplix utils]
            '[oplix.repl.core :only (repl clear-repl-content)])
       (when (.exists replrc)
         (load-file (str replrc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-data-dir
  []
  (with-create-on-get-dir
   (str (get-library-oplix-dir 'incantea.data))))

(defn get-examples-dir
  []
  (with-create-on-get-dir
   (str (get-library-oplix-dir 'incantea.examples))))

(defn get-data-file
  [file-name]
  (let [fp (File. (get-data-dir) (str file-name))]
    (when (.exists fp)
      (str fp))))

(defn get-example-file
  [file-name]
  (let [fp (File. (get-examples-dir) (str file-name))]
    (when (.exists fp)
      (str fp))))
