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

(ns oplix.api-browser.java
  (:use [clojure.java browse]
        [openar config log]
        [oplix.api-browser core])
  (:import (java.io BufferedReader File FileReader InputStreamReader)
           (java.net URI)
           (java.util.regex Pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-java-docs-api-urls
  []
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-java-api-url-map
  [url api-index api-symbol]
  "Return a map with href as key and a doc url as value for the api symbol."
  (try
    (let [[sym-ns sym-name] (trim-api-symbol api-symbol)
          uri (URI. (str url api-index))
          rdr (BufferedReader. (if (= (.getScheme uri) "http")
                                 (InputStreamReader. (.openStream (.toURL uri)))
                                 (FileReader. (.toString (.getPath uri)))))
          rem (let [rep (Pattern/compile (str ".*<a href=\"(.+)\" title=\"(.+)\".*>" sym-name "<.*/a>.*")
                                         Pattern/CASE_INSENSITIVE)]
                (loop [lsq (line-seq rdr)]
                  (when (seq lsq)
                    (if-let [rem (re-matches rep (first lsq))]
                      rem
                      (recur (next lsq))))))]
      (if rem
        (let [hrf (second rem)
              ttl (last rem)]
          (if sym-ns
            (let [rep (re-pattern (str ".*\\s+" sym-ns "$"))]
              (if (re-matches rep ttl)
                {hrf (str url hrf)}
                {}))
            {hrf (str url hrf)}))
        {}))
    (catch Exception e
      (log-warning "api-browser: failed to get api url")
      (log-exception e)
      {})))

(defn get-java-api-urls
  [api-symbol]
  (when (load-api-doc-urls?)
    (when-let [uia (get-java-docs-api-urls)]
      (let [ums (map (fn [[url idx]]
                       (when (and url idx)
                         (get-java-api-url-map url idx api-symbol)))
                     uia)
            mum (reduce (fn [mm um]
                          (if (seq um)
                            (apply assoc mm (interleave (keys um) (vals um)))
                            mm))
                        {}
                        ums)
            uls (filter identity (map (fn [[k v]] (when k v)) mum))]
        (when (seq uls)
          uls)))))

(defn browse-java-api?
  [api-symbol]
  (if-let [urls (get-java-api-urls api-symbol)]
    (do
      (doseq [url urls]
        (browse-url url))
      true)
    false))
