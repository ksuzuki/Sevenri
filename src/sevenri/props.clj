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

(ns sevenri.props
  "Sevenri unified properties lib"
  (:use [sevenri config defs log])
  (:import (java.io File)
           (java.io FileInputStream InputStreamReader)
           (java.io BufferedWriter FileOutputStream OutputStreamWriter)
           (java.util Properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Properties
;;
;; Unlike Java's property which is defined as both key and value are string,
;; Sevenri's property is a pair of string as key and any object as value,
;; and the values persist in UTF-8 str form.
;; It's the user's responsibility to remember which property has a non-string
;; object value. For reading a saved value back as a Clojure object, the
;; 'read-prop' macro is provided as shorthand.
;; Key can be either a symbol or a string which consists of valid chars for
;; Java.util.Properties key.
;;
;; Property Identifier - pi
;; A pi is a symbol or a string that specifies a property in a form which
;; uses dot to denote difference property domains. For example, the pi of
;; the current Sevenri's system instance directory name is
;; 'sevenri.sid.name'.
;;
;; Property Functions
;; Property fns operate on properties object returned from either
;; 'get-props' or 'get-slix-props'.
;; 'get-prop' can take a not-found value and it is returned when the
;; querying property doesn't exist. When querying property doesn't exist
;; *and* not-found value is not specifed, nil is returned. Either way an
;; item for the key won't be created on the properties object unless a value
;; is put with the key using 'put-prop'.
;; Property values won't persist unless they are saved to properties object
;; by 'save-prop' and then saved to storage mediumn by
;; 'store-persistent-props'. Also use 'save-prop' when you want to make a
;; change to JVM properties (the ones which are accessed by
;; System/getProperty).
;;
;; Property Domains, Files and Load Order
;; There are three property domains; :sevenri, :slix, and :frame. The
;; :sevenri domain properties are the ones of the JVM and Sevenri and
;; 'get-props' returns the properties object to access them. The :slix
;; domain Properties are the ones of slix and 'slix-props' returns the
;; properites object to access them. The :frame domain properties are
;; actually a subset of :slix domain properties but are the ones considered
;; as frame's properties and 'frame-props' returns the properties object to
;; access them.
;; When properties object is created, default, user defined, and any
;; persistent propertiy key/values are pre-loaded from files named as
;; 'default.properties', 'user.properties', and 'persistent.properties' in
;; that order. Pesistent properties are the ones put to properties object
;; using 'save-prop' and written out to the file when slix is closed and
;; Sevenri shuts down.
;; :frame properties are exceptional; no property loading and saving are
;; done for :frame properties objects.
;;
;; Implementaion Notes
;; * Getting and putting values are actually handled by the 'get' and 'put'
;;   methods of java.util.Properties. Putting non-string values make props
;;   'compromised', but it's OK because the 'store' method won't be
;;   performed on props.
;; * Saved values are stored in a ref map, which can be extracted using
;;   'get-saved'
;; * slix and frame Properties fns are defined in slix.clj.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PProperties
  (get-prop [props pi] [props pi not-found-val])
  (put-prop [props pi val])
  (save-prop [props pi val])
  (remove-prop [props pi])
  ;;
  (prop-keys [props])
  (prop-vals [props])
  (prop-count [props])
  (prop-seq [props])
  ;;
  (get-native [props])
  (get-saved [props])
  (get-info [props])
  ;;
  (load-props [props path file-name])
  (load-persistent-props [props path file-name])
  (store-persistent-props [props path file-name]))

(defn get-props
  []
  *properties*)

;;;;

(defmacro read-prop
  ([props pi]
     `(read-string (get-prop ~props ~pi)))
  ([props pi nfval]
     `(read-string (get-prop ~props ~pi ~nfval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(using-fns props core
           [trash-path?])

(def *sevenri-pi-prefixes* (map #(str "^" % \.) (get-config 'src.top-level-ns)))

(defmacro is-sevenri-pi?
  [pi]
  `(if (some #(zero? (.indexOf (str ~pi) %)) *sevenri-pi-prefixes*) true false))

;;;;

(defn get-prop*
  ([^java.util.Properties props pi]
     (.get props (str pi)))
  ([^java.util.Properties props pi nfval]
     (.get props (str pi) nfval)))

(defn put-prop*
  ([^java.util.Properties props pi val]
     (.put props (str pi) val)))

(defn save-prop*
  [^java.util.Properties props pi val saved]
  (let [key (str pi)]
    (when-not (is-sevenri-pi? key)
      (System/setProperty key (str val)))
    (reset! saved (assoc @saved key val))
    (.put props key val)))

(defn remove-prop*
  [^java.util.Properties props pi saved]
  (let [key (str pi)]
    (reset! saved (dissoc @saved key))
    (.remove props key)))

(defn prop-seq*
  [props]
  (if (map? props)
    (seq props)
    (when (and (instance? java.util.Properties props) (pos? (count props)))
      (map #(clojure.lang.MapEntry. (.getKey %) (.getValue %)) props))))

(defn load-props*
  [^java.util.Properties props path file-name]
  (let [file (File. (str path) (str file-name))]
    (when (.exists file)
      (with-open [reader (InputStreamReader. (FileInputStream. file) "UTF-8")]
        (try
          (.load props reader)
          file
          (catch Exception e
            (log-severe "load-props* failed:" file)
            nil))))))

(defn load-persistent-props*
  [^java.util.Properties props path file-name saved]
  (let [file (File. (str path) (str file-name))]
    (when (.exists file)
      (with-open [reader (InputStreamReader. (FileInputStream. file) "UTF-8")]
        (try
          (let [temp-props (Properties.)]
            (.load temp-props reader)
            (loop [keys (enumeration-seq (.keys temp-props))
                   temp-saved {}]
              (if (seq keys)
                (let [key (first keys)
                      val (.get temp-props key)]
                  (.put props key val)
                  (recur (rest keys) (assoc temp-saved key val)))
                (reset! saved (merge @saved temp-saved)))))
          file
          (catch Exception e
            (log-severe "load-persistent-props* failed:" file)
            nil))))))

(defn store-persistent-props*
  [saved path file-name]
  (when-let [kvs (seq (merge (sorted-map) @saved))]
    ;; There is something to persist.
    (let [path (File. (str path))
          file (File. path (str file-name))
          ;; normalize keyword string: escape '=' and ':'
          nrmk (fn [k] (.replace (.replace (str k) "=" "\\=") ":" "\\:"))]
      (when (if (or (.exists path) (.mkdirs path))
              true
              (do
                (log-severe "store-persistent-props*: mkdirs failed:" path)
                false))
        ;; The path exists.
        (when (if (and (.exists file) (not (props-using-trash-path?-core file)))
                (do
                  (log-severe "store-persistent-props*: trash-path? failed:" file)
                  false)
                true)
          ;; The file is clear and ready to be written.
          (with-open [writer (BufferedWriter. (OutputStreamWriter. (FileOutputStream. file) "UTF-8"))]
            (try
              (loop [kvs kvs]
                (when (seq kvs)
                  (let [[key val] (first kvs)
                        key-str (nrmk key)
                        val-str (str val)
                        kv-line (format "%s=%s\n" key-str val-str)]
                    (.write writer kv-line 0 (count kv-line))
                    (recur (rest kvs)))))
              file
              (catch Exception e
                (log-severe "store-persistent-props* failed:" file)
                nil))))))))

;;;;

(defn create-properties*
  ([]
     (create-properties* (Properties.)))
  ([props]
     (let [props (if props
                   (if (instance? Properties props)
                     props
                     (if (satisfies? PProperties props)
                       (get-native props)
                       (throw (IllegalArgumentException. "create-properties*: invalid props:" props))))
                   (Properties.))
           saved (atom {})]
       (reify PProperties
         (get-prop [_ pi] (get-prop* props pi))
         (get-prop [_ pi nfval] (get-prop* props pi nfval))
         (put-prop [_ pi val] (put-prop* props pi val))
         (save-prop [_ pi val] (save-prop* props pi val saved))
         (remove-prop [_ pi] (remove-prop* props pi saved))
         ;;
         (prop-keys [_] (enumeration-seq (.keys props)))
         (prop-vals [_] (iterator-seq (.iterator (.values props))))
         (prop-count [_] (.size props))
         (prop-seq [_] (prop-seq* props))
         ;;
         (get-native [_] props)
         (get-saved [_] saved)
         (get-info [_] {:domain :sevenri})
         ;;
         (load-props [_ path file-name] (load-props* props path file-name))
         (load-persistent-props [_ path file-name] (load-persistent-props* props path file-name saved))
         (store-persistent-props [_ path file-name] (store-persistent-props* saved path file-name))
         ;;
         (toString [this] (str "Properties[domain=:sevenri#" (.hashCode this) "]"))))))

;;;;

(defn create-jvm-properties
  []
  (create-properties* (System/getProperties)))

(defn create-sevenri-properties
  []
  (let [props (create-jvm-properties)]
    ;; src.properties.*.default
    (let [prop-path (reduce (fn [p c] (File. p (str c)))
                            (get-user-dir)
                            [(get-config 'src.dir)
                             (get-config 'src.properties.dir)])
          file-name (get-config 'src.properties.default-file-name)]
      (doseq [dir (get-config 'src.top-level-ns)]
        (let [path (File. prop-path (str dir))]
          (load-props props path file-name))))
    ;; dsr.properties.user
    (let [dsr-path (File. *dsr-path* (str (get-config 'dsr.properties.dir)))]
      (load-props props dsr-path (get-config 'dsr.properties.user-file-name)))
    ;; sid.properties.user and sid.properties.persistent
    (let [sid-path (File. *sid-path* (str (get-config 'sid.properties.dir)))]
      (load-props props sid-path (get-config 'sid.properties.user-file-name))
      (load-persistent-props props sid-path (get-config 'sid.properties.persistent-file-name)))
    ;;
    props))

(defn save-sevenri-properties
  [props]
  ;; sid.properties.persistent
  (let [sid-path (File. *sid-path* (str (get-config 'sid.properties.dir)))]
    (store-persistent-props props sid-path (get-config 'sid.properties.persistent-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn- -setup-properties?
  []
  (try
    (redef! *properties* (create-sevenri-properties))
    true
    (catch Exception e
      (log-severe "-setup-properties? failed:\n" (get-stack-trace-print-lines e))
      false)))

(defn- -save-properties?
  []
  (save-sevenri-properties (get-props))
  true)

;;;;

(defn startup-props?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -setup-properties?)))

(defn shutdown-props?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -save-properties?)))
