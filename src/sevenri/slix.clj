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

(ns ^{:doc "slix - Sevenri library complex"}
  sevenri.slix
  (:use [sevenri config core defs event log jvm os props refs ui utils])
  (:import (java.awt.event KeyAdapter KeyEvent)
           (java.beans ExceptionListener XMLEncoder XMLDecoder)
           (java.io BufferedOutputStream BufferedInputStream
                    File FileFilter FileInputStream FileOutputStream
                    InputStreamReader PushbackReader)
           (java.net URL URLClassLoader)
           (java.util Properties)
           (javax.swing JFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *slix* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PSlix
  "Protocol to deal with slix object"
  (get-slix-sn [slix] "Return the slix name, a symbol.")
  (get-slix-name [slix] "Return the slix instance name, a string.")
  (get-slix-args [slix] "Return the arguments to the slix")
  ;;
  (get-slix-id [slix] "Return the unique symbol assigned to the slix")
  (get-slix-cl [slix] "Return the class loader of the slix")
  ;;
  (get-slix-context [slix] "Return the context of the slix")
  (get-slix-props [slix] "Return the properties of the slix (a part of the slix context)")
  (get-slix-public [slix] "Return the public info of the slix (a part of the slix context)")
  ;;
  (get-slix-frame [slix] "Return the JFrame associated with the slix")
  ;;
  (get-slix-map [slix] "Return the slix as map"))

(defn is-slix?
  [obj]
  (satisfies? PSlix obj))

;;;;

(defn reify-slix*
  "smap is a map with key-vals corresponding to methods."
  [smap]
  (reify PSlix
    (get-slix-sn [_] (:sn smap))
    (get-slix-name [_] (:name smap))
    (get-slix-args [_] (:args smap))
    ;;
    (get-slix-id [_] (:id smap))
    (get-slix-cl [_] (:cl smap))
    ;;
    (get-slix-context [_] (:context smap))
    (get-slix-props [_] (:properties (:context smap)))
    (get-slix-public [_] (:public (:context smap)))
    ;;
    (get-slix-frame [_] (:frame smap))
    ;;
    (get-slix-map [_] smap)
    ;;
    (toString [_] (str "Slix " (:sn smap)
                       "[name=\"" (:name smap) "\""
                       ",args=[" (:args smap) "]"
                       ",id=" (:id smap)
                       ",cl=" (:cl smap)
                       ",context=[" (:context smap) "]"
                       ",frame=[" (:frame smap) "]]"))))

;;;;

(defmacro def-slix-fn*
  "Define slix-x fn which invokes corresponding PSlix method with *slix* or
   a given slix."
  [name]
  (let [fn-name# (symbol (str 'slix- name))
        method-name# (symbol (str 'get- fn-name#))]
    `(defn ~fn-name#
       ([] (~method-name# ~'*slix*))
       ([~'slix] (~method-name# ~'slix)))))

(def-slix-fn* sn)
(def-slix-fn* name)
(def-slix-fn* args)
(def-slix-fn* id)
(def-slix-fn* cl)
(def-slix-fn* context)
(def-slix-fn* props)
(def-slix-fn* public)
(def-slix-fn* frame)
(def-slix-fn* map)

;;;;

(defmulti get-slix
  (fn [obj]
    (cond
     (is-slix? obj) :slix
     (instance? JFrame obj) :frame
     :else :default)))

(defmethod get-slix :slix
  [slix]
  (when (identical? slix (get @*slixes* (slix-name slix)))
    slix))

(defmethod get-slix :frame
  [frame]
  (.getClientProperty (.getRootPane frame) '*slix*))

(defmethod get-slix :default
  [obj]
  (get @*slixes* (str obj)))

;;;;

(defn get-slix-ns
  "Return slix namespace name, a symbol. The first argument is either slix
   or slix-sn. The rest of the argument, if any, is sub namespace name."
  ([slix-or-sn]
     (symbol (str "slix." (if (is-slix? slix-or-sn) (slix-sn slix-or-sn) slix-or-sn))))
  ([slix-or-sn sub-ns]
     (get-slix-ns (str (if (is-slix? slix-or-sn) (slix-sn slix-or-sn) slix-or-sn) \. sub-ns)))
  ([slix-or-sn sub-ns & sub-nss]
     (apply get-slix-ns slix-or-sn (str sub-ns \. (first sub-nss)) (rest sub-nss))))

(defn get-slixes
  ([]
     (vals @*slixes*))
  ([sn]
     (seq (filter #(= (symbol (str sn)) (slix-sn %)) (get-slixes)))))

(defn get-slix-names
  []
  (keys @*slixes*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-src-slix-path
  "Return the path to the slix directory when no arguments are given.
   When arguments are given, the first one should a slix name in symbol or
   string (and sym2path is applied to it either way). The rest of the
   arguments are optional path specifiers."
  ([]
     (get-src-path (get-config 'src.slix.dir)))
  ([sn & paths]
     (apply get-src-path (get-slix-ns sn) paths)))

(defmacro get-slix-path
  "Shorthand of get-src-slix-path"
  ([]
     `(get-src-slix-path))
  ([sn & paths]
     `(get-src-slix-path ~sn ~@paths)))

(defn get-src-library-slix-path
  "Return the path to the library/slix directory when no arguments are
   given. When arguments are given, the first one should a slix name in
   symbol or string (and sym2path is applied to it either way). The rest of
   the arguments are optional path specifiers."
  ([]
     (get-src-library-path (get-config 'src.library.slix.dir)))
  ([sn & paths]
     (apply get-src-library-path (get-slix-ns sn) paths)))

(defmacro get-library-slix-path
  "Shorthand of get-src-library-slix-path"
  ([]
     `(get-src-library-slix-path))
  ([sn & paths]
     `(get-src-library-slix-path ~sn ~@paths)))

;;;;

(defn get-sid-classes-slix-path
  "Return the path to the sid/classes/slix directory when no arguments are
   given. When arguments are given, the first one should a slix name in
   symbol or string (and sym2path is applied to it either way). The rest of
   the arguments are optional path specifiers."
  [sn & paths]
  (apply get-sid-classes-path (get-slix-ns sn) paths))

(defn get-sid-slix-path
  "Return the path to the sid/slix directory when no arguments are given.
   When arguments are given, the first one should a slix name in symbol or
   string (and sym2path is applied to it either way). The rest of the
   arguments are optional path specifiers."
  ([]
     (get-sid-path (get-config 'sid.slix.dir)))
  ([sn & paths]
     (apply get-sid-path (get-slix-ns sn) paths)))

(defn get-sid-slix-save-path
  "Return the path to the -save- directory of the designated slix. the first
   argument should a slix name in symbol or string (and sym2path is applied
   to it either way). The rest of the arguments are optional path
   specifiers."
  [sn & paths]
  (apply get-sid-slix-path sn (get-config 'sid.slix.save.dir) paths))

(defn get-sid-slix-name-path
  "Return the path to the designated slix instance directory. Call this
   function either with a single slix object or with a slix name and an
   instance name. The slix name is either in symbol or string (and sym2path
   is applied to it)."
  ([slix]
     (get-sid-slix-name-path (slix-sn slix) (slix-name slix)))
  ([sn name]
     (get-sid-slix-save-path sn (str name))))

 (defn get-slix-startup-file
   "Return the path to the slix startup file."
   []
   (get-sid-path (get-config 'sid.slix.dir) (get-config 'sid.slix.startup.file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PPropertyListener
  "Protocol for managing property listener"
  (add-listener [props pi slix listener]
    "Add the listener owned by the slix, which is called when the value of
     the property specified by the pi is changed.")
  (remove-listener [props pi slix listener] [props pi slix] [props pi-or-slix]
    "Remove the listener owned by the slix and associated to the property
     specified by the pi, all listeners owned by the slix for the pi, all
     listeners owned by the slix for all pis, or all slix-listeners for the
     pi.")
  (get-listeners [props pi slix] [props pi-or-slix]
    "Return a set of listeners owned by the slix for the specified pi, the
     slix-listeners map of the specified pi, or a seq of [pi listeners]
     of the specified slix for all pis."))

;; pi-slix-listeners structure
;; pi-slix-listeners :=
;; {piA {slixA #{listenerA listenerB ...}
;;       slixB #{listenerA listenerB ...}
;;         :              :
;;       slixX #{listenerA listenerB ...}}
;;
;;  piB {slixA #{listenerA listenerB ...}
;;       slixB #{listenerA listenerB ...}
;;         :              :
;;       slixX #{listenerA listenerB ...}}
;;   :     :              :
;;  piX {slixA #{listenerA listenerB ...}
;;       slixB #{listenerA listenerB ...}}
;;         :              :
;;       slixX #{listenerA listenerB ...}}}

(defn add-listener*
  [pi slix listener pi-slix-listeners]
  (if (and pi (is-slix? slix)
           (fn? (if (var? listener) (var-get listener) listener)))
    ;; All params are OK.
    (if-let [slix-listeners (get pi-slix-listeners pi)]
      ;; There is a slix-listeners map for pi.
      (assoc pi-slix-listeners
        pi (assoc slix-listeners
             slix (conj (get slix-listeners slix) listener)))
      ;; No slix-listeners associated to pi yet.
      (assoc pi-slix-listeners pi {slix #{listener}}))
    ;; Invalid params. Return pi-slix-listeners untouched.
    pi-slix-listeners))

(defn remove-listener*
  ([pi slix listener pi-slix-listeners]
     (if (and pi (is-slix? slix)
              (or (fn? listener) (var? listener)))
       (if-let [slix-listeners (get pi-slix-listeners pi)]
         ;; There is a slix-listeners map for pi.
         (let [listeners (disj (get slix-listeners slix) listener)]
           (if (empty? listeners)
             ;; No listeners owned by slix
             (let [slix-listeners (dissoc slix-listeners slix)]
               (if (empty? slix-listeners)
                 ;; No slix-listeners for pi
                 (dissoc pi-slix-listeners pi)
                 ;; There's slix-listeners still.
                 (assoc pi-slix-listeners pi slix-listeners)))
             ;; There's listeners owned by slix still.
             (assoc pi-slix-listeners
               pi (assoc slix-listeners slix listeners))))
         ;; Unknown pi
         pi-slix-listeners)
       ;; Invalid params. Return pi-slix-listeners untouched.
       pi-slix-listeners))
  ([pi slix pi-slix-listeners]
     (if (and pi (is-slix? slix))
       (if-let [slix-listeners (get pi-slix-listeners pi)]
         ;; There is a slix-listeners map for pi.
         (let [slix-listeners (dissoc slix-listeners slix)]
           (if (empty? slix-listeners)
             ;; No slix-listeners for pi
             (dissoc pi-slix-listeners pi)
             ;; There's slix-listeners still.
             (assoc pi-slix-listeners pi slix-listeners)))
         ;; Unknown pi
         pi-slix-listeners)
       ;; Invalid params. Return pi-slix-listeners untouched.
       pi-slix-listeners))
  ([pi-or-slix pi-slix-listeners]
     (if (is-slix? pi-or-slix)
       ;; Remove slix-listeners for each pi.
       (let [slix pi-or-slix]
         (reduce (fn [psl pi] (remove-listener* pi slix psl))
                 pi-slix-listeners
                 (keys pi-slix-listeners)))
       ;; Remove pi and associated slix-listeners.
       (let [pi pi-or-slix]
         (dissoc pi-slix-listeners pi)))))

(defn get-listeners*
  ([pi slix pi-slix-listeners]
     (when (and pi (is-slix? slix))
       (get (get pi-slix-listeners pi) slix)))
  ([pi-or-slix pi-slix-listeners]
     (if (is-slix? pi-or-slix)
       (let [slix pi-or-slix]
         ;; Return a seq of [pi listeners] for specified slix for all pis.
         (seq (remove nil? (map #(when-let [listeners (get (get pi-slix-listeners %) slix)]
                                   [% listeners])
                                (keys pi-slix-listeners)))))
       (let [pi pi-or-slix]
         ;; Return the slix-listeners map of the pi.
         (get pi-slix-listeners pi)))))

(defn put-prop**
  "Update the prop specified by pi. Then invoke listeners associated to it.
   Remove the listener when failed on its invokation, or the entire listeners
   if the slix owning them is invalid."
  [props pi val sn name ref-pi-slix-listeners]
  (let [old (.put props (str pi) val)]
    (when (contains? @ref-pi-slix-listeners pi)
      ;; There are listeners on pi.
      (let [event {:sn sn :name name :pi pi :old old :new val}]
        (doseq [slix-listeners (get @ref-pi-slix-listeners pi)]
          (let [[slix listeners] slix-listeners]
            (declare get-slix)
            (if (get-slix slix)
              (doseq [listener listeners]
                (when-not (try (binding [*slix* slix] (listener event))
                               true
                               (catch Exception e
                                 (log-warning "put-prop** exception on event:" event
                                              "\n" (get-stack-trace-print-lines e))
                                 false))
                  ;; Get rid of broken listener.
                  (reset! ref-pi-slix-listeners (remove-listener* pi slix listener @ref-pi-slix-listeners))))
              ;; Invalid slix. Get rid of it and its listeners.
              (reset! ref-pi-slix-listeners (remove-listener* pi slix @ref-pi-slix-listeners)))))))
    old))

;;;;

(defn create-slix-properties*
  [sn name]
  (let [props (Properties.)
        saved (atom {})
        pslmp (atom {})] ;; plmap := pi-slix-listeners map
    (reify
      PProperties
      (get-prop [_ pi] (get-prop* props pi))
      (get-prop [_ pi nfval] (get-prop* props pi nfval))
      (put-prop [_ pi val] (put-prop** props pi val sn name pslmp))
      (save-prop [this pi val] (do
                                 (reset! saved (assoc @saved (str pi) val))
                                 (put-prop this pi val)))
      (remove-prop [_ pi] (let [key (str pi)]
                            (reset! saved (dissoc @saved key))
                            (.remove props key)))
      ;;
      (prop-keys [_] (enumeration-seq (.keys props)))
      (prop-vals [_] (iterator-seq (.iterator (.values props))))
      (prop-count [_] (.size props))
      (prop-seq [_] (prop-seq* props))
      ;;
      (get-native [_] props)
      (get-saved [_] saved)
      (get-info [_] {:domain :slix :sn sn :name name})
      ;;
      (load-props [_ path file-name] (load-props* props path file-name))
      (load-persistent-props [_ path file-name] (load-persistent-props* props path file-name saved))
      (store-persistent-props [_ path file-name] (store-persistent-props* saved path file-name))
      ;;
      PPropertyListener
      (add-listener [_ pi slix listener] (reset! pslmp (add-listener* pi slix listener @pslmp)))
      (remove-listener [_ pi slix listener] (reset! pslmp (remove-listener* pi slix listener @pslmp)))
      (remove-listener [_ pi slix] (reset! pslmp (remove-listener* pi slix @pslmp)))
      (remove-listener [_ pi-or-slix] (reset! pslmp (remove-listener* pi-or-slix @pslmp)))
      (get-listeners [_ pi slix] (get-listeners* pi slix @pslmp))
      (get-listeners [_ pi-or-slix] (get-listeners* pi-or-slix @pslmp))
      ;;
      (toString [this] (str "Properties[domain=:slix#" (.hashCode this) ",sn=" sn ",name=" name "]")))))

(defn create-slix-properties
  "Create the properties object local for a slix and load the default, user,
   and persistent properties of the slix."
  ([slix]
     (when (is-slix? slix)
       (create-slix-properties (slix-sn slix) (slix-name slix))))
  ([sn name]
     (let [props (create-slix-properties* sn name)]
       ;; src.slix.sn.properties.default
       (let [src-sn-path (get-src-slix-path sn (get-config 'src.slix.properties.dir))]
         (load-props props src-sn-path (get-config 'src.slix.properties.default-file-name)))
       ;; sid.slix.sn.properties.user
       (let [sid-sn-path (get-sid-slix-path sn (get-config 'sid.slix.properties.dir))]
         (load-props props sid-sn-path (get-config 'sid.slix.properties.user-file-name)))
       ;; sid.slix.sn.-save-.name.properties.persistent
       (let [sid-sn-name-path (get-path (get-sid-slix-name-path sn name)
                                        (get-config 'sid.slix.properties.dir))]
         (load-persistent-props props sid-sn-name-path (get-config 'sid.slix.save.persistent-file-name)))
       ;;
       props)))

(defn save-slix-properties
  "Save the persistent properties of a slix."
  ([]
     (save-slix-properties *slix*))
  ([slix]
     (when (is-slix? slix)
       (save-slix-properties (slix-sn slix) (slix-name slix) (slix-props slix))))
  ([sn name props]
     ;; sid.slix.sn.-save-.name.properties.persistent
     (let [sid-sn-name-path (get-path (get-sid-slix-name-path sn name)
                                      (get-config 'sid.slix.properties.dir))]
       (store-persistent-props props sid-sn-name-path (get-config 'sid.slix.save.persistent-file-name)))))

;;;;

(defn create-slix-frame-properties*
  [sn name]
  (let [props (atom {})]
    (reify PProperties
      (get-prop [_ pi] (pi @props))
      (get-prop [_ pi nfval] (get @props pi nfval))
      (put-prop [_ pi val] (let [v (get @props pi)] (reset! props (assoc @props pi val)) v))
      (save-prop [_ _ _] nil)
      (remove-prop [_ pi] (let [v (get @props pi)] (reset! props (dissoc @props pi)) v))
      ;;
      (prop-keys [_] (keys @props))
      (prop-vals [_] (vals @props))
      (prop-count [_] (count @props))
      (prop-seq [_] (seq @props))
      ;;
      (get-native [_] @props)
      (get-saved [_] nil)
      (get-info [_] {:domain :frame :sn sn :name name})
      ;;
      (load-props [_ _ _] nil)
      (load-persistent-props [_ _ _] nil)
      (store-persistent-props [_ _ _] nil)
      ;;
      (toString [this] (str "Properties[domain=:frame#" (.hashCode this) ",sn=" sn ",name=" name "]")))))

(defn add-slix-frame-properties
  [frame sn name]
  (.putClientProperty (.getRootPane frame) '*props* (create-slix-frame-properties* sn name)))

(defn frame-props
  [frame]
  (.getClientProperty (.getRootPane frame) '*props*))

(defmacro slix-frame-props
  ([]
     `(frame-props (slix-frame)))
  ([slix]
     `(frame-props (slix-frame ~slix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-to-slix-sn-cache
  [sn]
  (reset! *slix-sn-cache* (conj @*slix-sn-cache* sn)))

(defn remove-from-slix-sn-cache
  [sn]
  (reset! *slix-sn-cache* (disj @*slix-sn-cache* sn)))

(defn get-all-slix-sn
  []
  @*slix-sn-cache*)

(defn get-slix-name-from-ns
  [ns]
  (when-let [rm (re-matches #"^slix\.(.+)" (str ns))]
    (symbol (second rm))))

(defn get-slix-sn-meta
  [sn]
  (when-let [sns (find-ns (get-slix-ns sn))]
    (meta sns)))

(defn get-library-slix-ns
  [sn & syms]
  (symbol (str 'library \. (apply get-slix-ns sn syms))))

(defn get-slix-fn
  [sn sym]
  (ns-resolve (get-slix-ns sn) sym))

(defn find-all-slix-sn
  "Parse clj files (except scratch) and return slix namespaces without
   'slix.' prefix."
  []
  (let [slxdn (get-config 'src.slix.dir)
        slxdt (str slxdn \.)
        csxdt (count slxdt)
        rpclj (re-pattern (str "/" slxdn "/.*\\.clj$"))
        rpsct (re-pattern (str ".*/" (sym2path (get-config 'src.slix.scratch-file-name)) "$"))
        cljfs (filter #(not (re-matches rpsct (str %)))
                      (find-files #(re-find rpclj (str %)) (get-slix-path)))]
    (filter identity
            (map (fn [f]
                   (try
                     (with-open [rdr (PushbackReader. (InputStreamReader. (FileInputStream. f) "UTF-8"))]
                       (when-let [obj (read rdr)]
                         ;; Expect the ns macro line; (ns name docstring? attr-map? references*)
                         (let [sym1 (first obj) ;; ns
                               sym2 (second obj) ;; name
                               str2 (str sym2)]
                           (when (and (= 'ns sym1)
                                      (< csxdt (count str2))
                                      (= slxdt (subs str2 0 csxdt))
                                      (true? (:slix (meta sym2))))
                             (with-meta (symbol (subs str2 csxdt)) (meta sym2))))))
                     (catch Exception e
                       (log-severe "find-all-slix-sn failed:" f))))
                 cljfs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PSlixPublic
;; Slixes supporting this protocol define fns corresponding to 'get-public-x'
;; fn as 'public-x' in its slix-sn.public lib.

(defprotocol PSlixPublic
  "Protocol to extract details of slix public information"
  (get-public-props [pub]
    "Return a set of property name symbols that other slixes can safely
     refer and use when, for example, adding update notification handler on
     that property. Return nil when no such poperty exists.")
  (contain-public-prop? [pub name]
    "Return true when the public props contain the one specified by the name
     symbol. Otherwise, false.")
  ;;
  (get-public-opens [pub]
    "Return a map of data items being opened, such as paths, port numbers,
     subject names, or nil. Data item type keyword as key and a set of data
     items as val.")
  (get-public-open [pub type-kwd]
    "When the data item map contains the data items of the specified type,
     return the data item set. Otherwise, nil.")
  ;;
  (get-public-fns [pub]
    "Return a map of functions that other slixes can call, or nil. Function
     name symbol (not fully qualified) as key and function var or object as
     val.")
  (get-public-fn [pub name]
    "When the function map contains the function specified by the name
     symbol, return the function var or object. Otherwise, nil."))

;;;;

(defn get-slix-public-fn*
  "Return the fn defined as return-public-kind in the slix.sn.public lib, or
   nil."
  [sn kind]
  (let [pub-lib (get-slix-ns sn 'public)
        pub-sym (symbol (str 'public- kind))]
    (when (find-ns pub-lib)
      (ns-resolve pub-lib pub-sym))))

(defn call-slix-public-fn*
  [sn name f]
  (when-let [slix (get-slix name)]
    (when (= sn (slix-sn slix))
      (binding [*slix* slix]
        (f)))))

(defn create-slix-public
  [sn name]
  (reify PSlixPublic
    (get-public-props [_] (when-let [f (get-slix-public-fn* sn 'props)]
                            (when-let [s (call-slix-public-fn* sn name f)]
                              (when (set? s)
                                s))))
    (contain-public-prop? [this name] (when-let [props (get-public-props this)]
                                        (contains? props name)))
    ;;
    (get-public-opens [_] (when-let [f (get-slix-public-fn* sn 'opens)]
                            (when-let [m (call-slix-public-fn* sn name f)]
                              (when (map? m)
                                m))))
    (get-public-open [this type-kwd] (when-let [opens (get-public-opens this)]
                                       (get opens type-kwd)))
    ;;
    (get-public-fns [_] (when-let [f (get-slix-public-fn* sn 'fns)]
                          (when-let [m (call-slix-public-fn* sn name f)]
                            (when (map? m)
                              m))))
    (get-public-fn [this name] (when-let [fns (get-public-fns this)]
                                 (get fns name)))
    ;;
    (toString [this] (str "SlixPublic " sn "[name=\"" name "\""
                          ",props=" (get-public-props this)
                          ",opens=" (get-public-opens this)
                          ",fn=" (get-public-fns this) "]"))))

;;;;

(defn find-slix-with-public-open-item
  ([type-kwd item]
     (find-slix-with-public-open-item type-kwd item nil))
  ([type-kwd item sn]
     (when (and (keyword? type-kwd) item)
       (loop [slixes (if sn (get-slixes sn) (get-slixes))]
         (when-let [slix (first slixes)]
           (let [items (get-public-open (slix-public slix) type-kwd)]
             (if (contains? items item)
               slix
               (recur (rest slixes)))))))))

(defn find-slix-with-public-fn
  ([name]
     (find-slix-with-public-fn name nil))
  ([name sn]
     (when name
       (loop [slixes (if sn (get-slixes sn) (get-slixes))]
         (when-let [slix (first slixes)]
           (when-let [fmap (get-public-fns (slix-public slix))]
             (if (get fmap (symbol name))
               slix
               (recur (rest slixes)))))))))

(defn invoke-slix-public-fn
  [slix fnsym & params]
  (when (and (is-slix? slix) fnsym)
    (when-let [fnvar (get-public-fn (slix-public slix) (symbol fnsym))]
      (when (and (var? fnvar) (fn? (var-get fnvar)))
        (binding [*slix* slix]
          (apply (var-get fnvar) params))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sid-slix-frame-file
  ([]
     (get-sid-slix-frame-file *slix*))
  ([slix]
     (get-sid-slix-frame-file (slix-sn slix) (slix-name slix)))
  ([sn name]
     (get-sid-slix-frame-file sn name (get-config 'sid.slix.save.frame-file-name)))
  ([sn name frame-file-name]
     (let [dir (with-make-path (get-sid-slix-name-path sn name))]
       (get-path dir frame-file-name))))

(defn get-sid-slix-state-file
  ([]
     (get-sid-slix-state-file *slix*))
  ([slix]
     (get-sid-slix-state-file (slix-sn slix) (slix-name slix)))
  ([sn name]
     (get-sid-slix-state-file sn name (get-config 'sid.slix.save.state-file-name)))
  ([sn name state-file-name]
     (let [dir (with-make-path (get-sid-slix-name-path sn name))]
       (get-path dir state-file-name))))

(defn get-sid-slix-file-bundle
  "Return [frame-file state-file] or nil"
  ([]
     (get-sid-slix-file-bundle *slix*))
  ([slix]
     (get-sid-slix-file-bundle (slix-sn slix) (slix-name slix)))
  ([sn name]
     [(get-sid-slix-frame-file sn name) (get-sid-slix-state-file sn name)]))

(defn get-sid-slix-file-bundle-saved
  "Return [frame-file/nil state-file/nil] or nil."
  ([]
     (get-sid-slix-file-bundle-saved *slix*))
  ([slix]
     (get-sid-slix-file-bundle-saved (slix-sn slix) (slix-name slix)))
  ([sn name]
     (let [[f s] (get-sid-slix-file-bundle sn name)
           frame-file (when (.exists f) f)
           state-file (when (.exists s) s)]
       (when (or frame-file state-file)
         [frame-file state-file]))))

(defmacro slix-file-bundle
  "Shorthand of get-sid-slix-file-bundle."
  ([] `(get-sid-slix-file-bundle))
  ([slix] `(get-sid-slix-file-bundle ~slix))
  ([sn name] `(get-sid-slix-file-bundle ~sn ~name)))

;;;;

(defn is-slix-saved?
  "Return true when slix bundle file(s) is saved."
  ([]
     (is-slix-saved? *slix*))
  ([slix]
     (is-slix-saved? (slix-sn slix) (slix-name slix)))
  ([sn name]
     (if (get-sid-slix-file-bundle-saved sn name)
       true
       false)))

(defn get-saved-slix-names
  "Return a seq of names or nil"
  [sn]
  (when-let [od (get-sid-slix-save-path sn)]
    (let [ff (proxy [FileFilter] []
               (accept [p] (not (empty-path? p))))]
      (seq (map #(.getName %) (.listFiles od ff))))))

(defn find-saved-slixes
  "Return a seq of [sn name [frame-file state-file/nil]] or nil."
  ([]
     (let [;; Find any files under sis/slix, convert them in string, and then sort them.
           afps (sort (map str (find-files #(.isFile %) (get-sid-slix-path))))
           ;; Remove up to sis/slix and go to the next stage.
           -sv- (get-config 'sid.slix.save.dir)
           rptn (re-pattern (str "^" (get-sid-slix-path) "/(.*/" -sv- "/.*)$"))]
       (find-saved-slixes (filter identity (map #(second (re-find rptn %)) afps))
                          (sym2path (get-config 'sid.slix.save.frame-file-name))
                          (sym2path (get-config 'sid.slix.save.state-file-name)))))
  ([snfiles ffname sfname]
     ;; Return a lazy-seq that consumes sn-name-files and return [sn name [f s/nil]]s.
     (lazy-seq
      (when (seq snfiles)
        (let [sn_name (.getParentFile (File. (first snfiles))) ;; sn/_save_/name
              sn (.getParent (.getParentFile sn_name))
              nm (.getName sn_name)
              create-item (fn [fs]
                            (let [fsv (vec (map #(File. (File. (get-sid-slix-path) (str sn_name)) %) fs))]
                              [(symbol (path2sym sn)) (str nm) (if (< (count fsv) 2) (conj fsv nil) fsv)]))]
          (loop [snfiles snfiles
                 file-bundle nil]
            (if (seq snfiles)
              ;; There are snfiles.
              (let [snfile (File. (first snfiles))
                    snfname (.getName snfile)]
                (if (= sn_name (.getParentFile snfile))
                  ;; Same sn/_save_/name. Add to file-bundle and loop.
                  (cond
                   (= snfname ffname) (recur (rest snfiles) (cons ffname file-bundle))
                   (= snfname sfname) (recur (rest snfiles) (if (= (first file-bundle) ffname)
                                                              (concat (list ffname sfname) (rest file-bundle))
                                                              (cons sfname file-bundle)))
                   ;; Currently ignore any files other than frame.xml or state.clj.
                   ;; If we want to list others too, do this:
                   ;;   :else (recur (rest snfiles) (concat file-bundle (list snfname))))
                   :else (recur (rest snfiles) file-bundle))
                  ;; Different sn/_save_/name.
                  (if (seq file-bundle)
                    ;; There is file-bundle. Construct [sn name [...]] and continue lazy-listing snfiles.
                    (cons (create-item file-bundle)
                          (find-saved-slixes snfiles ffname sfname))
                    ;; No file-bundle.
                    (find-saved-slixes snfiles ffname sfname))))
              ;; No more snfiles.
              (when (seq file-bundle)
                (list (create-item file-bundle))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-stdio
  []
  [*in* *out* *err*])

(defn get-base-class-loader
  []
  *base-class-loader*)

(defn get-system-event-queue
  []
  *system-event-queue*)

(defn get-slix-jvm-and-jar-paths
  [sn]
  (let [pa [(get-slix-path sn (get-config 'src.slix.jvm.dir))]]
    (reduce (fn [a p] (conj a p)) pa (find-files '.jar (first pa)))))

(defn get-slix-project-jar-paths
  [sn]
  (when-let [projman (get-project-manager)]
    (get-jars projman (get-slix-ns sn))))

(defn create-slix-class-loader
  [sn]
  (let [cps (let [s (conj (get-slix-jvm-and-jar-paths sn) (get-sid-classes-path))
                  p (get-slix-project-jar-paths sn)]
              (if p (apply conj s p) s))]
    (URLClassLoader. (into-array (map #(.toURL (.toURI %)) cps)) (get-base-class-loader))))

(defn load-slix-class
  [slix fqcn]
  (.loadClass (slix-cl slix) (str fqcn)))

(defn make-slix-class-instance
  [slix fqcn]
  (.newInstance (load-slix-class slix fqcn)))

(defmacro with-slix-context
  [sn slix-class-loader return-value-when-exception & body]
  `(let [ct# (Thread/currentThread)
         ccl# (.getContextClassLoader ct#)]
     (try
       (.setContextClassLoader ct# ~slix-class-loader)
       ~@body
       (catch Exception e#
         (log-exception e# (get-slix-ns ~sn))
         ~return-value-when-exception)
       (finally
        (.setContextClassLoader ct# ccl#)))))

(defn reload-sn?
  ([sn]
     (let [ns (get-slix-ns sn)]
       (require ns :reload)
       (if (find-ns ns)
         true
         false)))
  ([sn cl]
     (with-slix-context sn cl false
       (reload-sn? sn))))

(defn- -aot-compiler
  [sn aot verbose? cl-dump?]
  (let [aotf (get-path (get-slix-path sn) (str aot '.clj))]
    (if (.exists aotf)
      (let [cl (create-slix-class-loader sn)]
        (with-slix-context sn cl false
          (when cl-dump?
            (loop [cl cl]
              (when cl
                (println cl)
                (print-seq (seq (.getURLs cl)))
                (recur (.getParent cl)))))
          ;;
          (let [cp (get-sid-classes-path)
                fqaot (get-slix-ns sn aot)]
            (binding [*compile-path* (str cp)]
              (compile fqaot)
              true))))
      (let [s (str "aot-compile?: not found aot file: " aotf)]
        (when verbose?
          (log-warning s)
          (print-warning s))
        false))))

(defn aot-compile?
  ([sn]
     (aot-compile? sn 'aot true))
  ([sn aot]
     (aot-compile? sn aot true))
  ([sn aot verbose?]
     (aot-compile? sn aot verbose? false))
  ([sn aot verbose? cl-dump?]
     (let [-aot-compiler-bfn (bound-fn [] (-aot-compiler sn aot verbose? cl-dump?))
           ac (future (-aot-compiler-bfn))]
       @ac)))

(defn invoke-later
  "Take a body of expressions, post it to the event dispatch thread of the
   current or specified slix, and return with nil immedidately when wait?
   is false. The body will be evaluated later in the EDT."
  ([body]
     (invoke-later *slix* body))
  ([slix body]
     (invoke-later slix body false))
  ([slix body wait?]
     (when-not slix (throw (RuntimeException. "invoke-later: nil slix")))
     (binding [*slix* slix]
       (if-let [app-context (:app-context (slix-context slix))]
         (invoke-later-in-slix-context slix body wait?)
         (alt-invoke-later-in-slix-context slix body wait?)))
     nil))

(defn invoke-and-wait
  "Call invoke-later with true for wait?. Return nil."
  ([body]
     (invoke-and-wait *slix* body))
  ([slix body]
     (invoke-later slix body true)))

(defn is-depending-project-ready-if-exists?
  [sn]
  (let [projname (get-slix-ns sn)]
    (if-let [projman (get-project-manager)]
      (if (exists? projman projname)
        (built? projman projname)
        true)
      true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load/save slix frame
;;;; - DONOT CALL THESE FUNCTIONS DIRECTLY. CALL VIA INVOKE-LATER OR
;;;;   INVOKE-AND-WAIT.
;;;; - These save and load slix frame only. Saving and loading slix instance
;;;;   data should be done by the slix event handlers in response to save/load
;;;;   events.
;;;; - No need to setup per-slix CL because these are (have to be) called in
;;;;   slix's EDT where per-slix CL is set up.
;;;;
;;;; Note: make sure to use the same CL used for reload-sn?, or XMLEncoder
;;;; would go into infinite recursive calls.

(defn- -load-slix-frame
  ([slix]
     (-load-slix-frame (slix-sn slix) (slix-name slix)))
  ([sn name]
     (try
       (let [[f s] (get-sid-slix-file-bundle sn name)]
         (when (and (.exists f) (.canRead f))
           (with-open [xd (XMLDecoder. (BufferedInputStream. (FileInputStream. f)))]
             (.readObject xd))))
       (catch Exception e
         (log-exception e)
         nil))))

(defn- -presave-slix-frame
  [slix]
  (let [frame (slix-frame slix)
        presave-slix-frame-os-value (presave-slix-frame-os frame)]
    [#(postsave-slix-frame-os frame presave-slix-frame-os-value)]))

(defn- -postsave-slix-frame
  [postsave-slix-frame-fns]
  (doseq [pofn postsave-slix-frame-fns]
    (pofn)))

(defn- -save-slix-frame?
  [slix log-xml-encoder-errors?]
  (let [postsave-slix-frame-fns (-presave-slix-frame slix)]
    (try
      (let [[f s] (get-sid-slix-file-bundle (slix-sn slix) (slix-name slix))]
        (when (.exists f)
          (.delete f))
        (with-open [xe (XMLEncoder. (BufferedOutputStream. (FileOutputStream. f)))]
          ;;
          (set-event-delegator-persistence-delegate xe)
          ;; Ignore any exception if not log-xml-encoder-errors?.
          (when-not log-xml-encoder-errors?
            (.setExceptionListener xe (proxy [ExceptionListener][]
                                        (exceptionThrown [e]))))
          ;;
          (.writeObject xe (slix-frame slix))
          (.flush xe))
        true)
      (catch Exception e
        (log-exception e)
        false)
      (finally
       (-postsave-slix-frame postsave-slix-frame-fns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-singleton-slix?
  [obj]
  (let [sn (if (is-slix? obj)
             (slix-sn obj)
             (symbol (str obj)))
        fsn (filter #(= sn %) @*slix-sn-cache*)]
    (if (seq fsn)
      (true? (:singleton (get-slix-sn-meta (first fsn))))
      false)))

;;;;

(defn- -create-slix-context
  ([slix]
     (-create-slix-context (slix-sn slix) (slix-name slix) nil))
  ([sn name]
     (-create-slix-context sn name nil))
  ([sn name app-context]
     (let [context {:prop_ (ref {}) ;; Deprecated - removed by 0.3.0
                    :properties (create-slix-properties sn name)
                    :public (create-slix-public sn name)}]
       (if app-context
         (assoc context :app-context app-context)
         context))))

(defn- -slix-is-opening
  [name opening?]
  (dosync
   (ref-set *opening-slix-names* (if opening?
                                    (conj @*opening-slix-names* name)
                                    (disj @*opening-slix-names* name)))))

(defn- -is-slix-opening?
  ([name]
     (contains? @*opening-slix-names* name))
  ([name opening?]
     (if (contains? @*opening-slix-names* name)
       true
       (do
         (when opening?
           (-slix-is-opening name true))
         false))))

(defn- -create-initial-frame
  [slix]
  (let [f (JFrame.)
        n (slix-name slix)
        [w h] (read-prop (get-props) 'slix.frame.size)]
    (doto f
      (.setLocationByPlatform (read-prop (get-props) 'slix.frame.location-by-platform))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setTitle (str n))
      (.setSize w h))
    f))

(defn- -register-slix
  ([slix]
     (-register-slix slix (slix-name slix)))
  ([slix name]
     (dosync
      (ref-set *slixes* (assoc @*slixes* (str name) slix)))))

(defn- -unregister-slix
  ([slix]
     (-unregister-slix slix (slix-name slix)))
  ([slix name]
     (declare remove-from-xref) ;; deprecated - remove by 0.3.0
     (remove-from-xref slix)
     (dosync
      (ref-set *slixes* (dissoc @*slixes* (str name))))))

(defn- -abort-open-slix
  ([slix]
     (-abort-open-slix slix
                        :sevenri.event/slix-error-open
                        :sevenri.event/reason-exception-occurred))
  ([slix eid reason]
     (-abort-open-slix slix eid reason true))
  ([slix eid reason post-event?]
     (-slix-is-opening (slix-name slix) false)
     (when (get-slix slix)
       (-unregister-slix slix))
     (when-let [frame (slix-frame slix)]
       (.dispose frame))
     (when-let [app-context (:app-context (slix-context slix))]
       (dispose-app-context app-context))
     (when (empty-path? (get-sid-slix-name-path slix))
       (clean-path? (get-sid-slix-name-path slix) (get-sid-slix-path)))
     (when post-event?
       (post-event eid slix (if (and (map? reason) (:reason reason))
                               reason
                               {:reason reason})))
     (when (= reason :sevenri.event/reason-singleton-slix)
       (.toFront (slix-frame (first (get-slixes (slix-sn slix))))))
     eid))

(defmacro -send-event-and-continue-unless
  [deny-res slix eid send-fn & body]
  `(let [resps# (~send-fn ~eid ~slix)
         [res# rsn#] (get-event-response (get resps# (slix-name ~slix)))]
     (if (= res# :sevenri.event/response-exception-occurred)
       (-abort-open-slix ~slix)
       (if (and ~deny-res (= ~deny-res res#))
         (-abort-open-slix ~slix :sevenri.event/slix-open-canceled rsn#)
         (do
           ~@body)))))

(defn- -open-slix
  "Open slix synchronously."
  ([slix io]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [sn (slix-sn slix)
             name (slix-name slix)
             oeo-eid :sevenri.event/slix-error-open]
         (if (or (-is-slix-opening? name true) (get-slix name))
           ;; name exists
           (-abort-open-slix slix oeo-eid :sevenri.event/reason-name-exists)
           ;; continue opening
           (if (not (and (contains? (get-all-slix-sn) sn) (reload-sn? sn (slix-cl slix))))
             ;; reload-sn failed
             (-abort-open-slix slix oeo-eid :sevenri.event/reason-reload-sn-failed)
             ;; continue opening
             (if (and (is-singleton-slix? slix) (get-slixes sn))
               ;; singleton exists
               (-abort-open-slix slix oeo-eid :sevenri.event/reason-singleton-slix)
               ;; continue opening
               (let [saved? (is-slix-saved? slix)]
                 ;; opening
                 (-send-event-and-continue-unless
                  :sevenri.event/response-donot-open
                  slix :sevenri.event/slix-opening send-creation-event
                  (if saved?
                    ;; load frame
                    (let [frame (atom nil)
                          frame-loader #(reset! frame (-load-slix-frame slix))]
                      (-send-event-and-continue-unless
                       nil ;; cannot deny loading frame for now
                       slix :sevenri.event/slix-frame-loading send-creation-event
                       (invoke-and-wait slix frame-loader)
                       (if @frame
                         (-open-slix slix saved? @frame)
                         ;; load frame failed
                         (let [rsn :sevenri.event/reason-load-frame-failed]
                           (post-creation-event oeo-eid slix rsn)
                           (-abort-open-slix slix oeo-eid rsn false)))))
                    ;; create frame (never fail)
                    (let [frame (atom nil)
                          frame-creator #(reset! frame (-create-initial-frame slix))]
                      (-send-event-and-continue-unless
                       nil ;; cannot deny creating frame for now
                       slix :sevenri.event/slix-frame-creating send-creation-event
                       (invoke-and-wait slix frame-creator)
                       (-open-slix slix saved? @frame))))))))))))
  ([slix saved? frame]
     ;; Add frame properties.
     (add-slix-frame-properties frame (slix-sn slix) (slix-name slix))
     ;; Install the default listeners.
     (when-not saved?
       (doto frame
         (add-default-window-listener)
         (add-default-key-listener)))
     ;; Associate frame to slix.
     (let [slix (reify-slix* (assoc (slix-map slix) :frame frame))
           eid (if saved?
                 :sevenri.event/slix-frame-loaded
                 :sevenri.event/slix-frame-created)]
       ;; Refer to slix back from frame.
       (.putClientProperty (.getRootPane frame) '*slix* slix)
       ;; Notify frame creation or load.
       (-send-event-and-continue-unless
        nil ;; ignore any response
        slix eid send-creation-event
        (-register-slix slix)
        (-send-event-and-continue-unless
         nil ;; ditto
         slix :sevenri.event/slix-opened post-event
         ;; slix opened, finally.
         ;; If the frame is newly created, change the default close operation
         ;; to do nothing and let Sevenri handle the close operation.
         (when-not saved?
           (.setDefaultCloseOperation frame JFrame/DO_NOTHING_ON_CLOSE))
         (-slix-is-opening (slix-name slix) false)
         :sevenri.event/slix-opened)))))

(defn- -get-context-and-start-slix-creation
  ([slix]
     (let [sn (slix-sn slix)
           name (slix-name slix)]
       (if-let [app-context (create-app-context name (slix-cl slix))]
         ;; EDT per slix
         (-get-context-and-start-slix-creation slix (-create-slix-context sn name app-context))
         ;; sharing the same, main EDT
         (-get-context-and-start-slix-creation slix (-create-slix-context sn name)))))
  ([slix context]
     (let [slix (reify-slix* (assoc (slix-map slix) :context context))]
       (future
         (try
           (-open-slix slix (get-stdio))
           (catch Exception e
             (log-exception e)))))))

;;;;

(def *open-slix-args* nil)

(defn generate-slix-name
  ([sn]
     (generate-slix-name sn nil))
  ([sn pfx]
     (let [Name (str (apply str (.toUpperCase (str (first (str sn)))) (rest (str sn))) pfx)]
       (if-not (or (-is-slix-opening? Name) (get-slix Name))
         Name
         (loop [X 1]
           (let [NameX (str Name X)]
             (if-not (get-slix NameX)
               NameX
               (recur (inc X)))))))))

(defn open-slix
  "Return a future oject that creates a slix instance using slix name and
   notifies open events to it. Instance name is optional. If the opening
   slix requires depending project and it's not built, return a future
   object that delegates the open task to the project manager."
  ([sn]
     (open-slix sn (generate-slix-name sn)))
  ([sn name]
     (if (is-depending-project-ready-if-exists? sn)
       (let [sn (symbol sn)
             name (str name)
             cl (create-slix-class-loader sn)
             slix (reify-slix* {:id (gensym 'id) :sn sn :name name :cl cl :args *open-slix-args*})]
         (-get-context-and-start-slix-creation slix))
       (future
         (when-let [projman (get-project-manager)]
           (build-and-run projman (get-slix-ns sn) sn name *open-slix-args*)
           :sevenri.event/slix-open-after-building-project)))))

(defn open-slix-and-wait
  "Return the dereference to the future object returned from the open-slix
   call with slix name sn. Instance name is optional."
  ([sn]
     (open-slix-and-wait sn (generate-slix-name sn)))
  ([sn name]
     (let [opener (open-slix sn name)]
       @opener)))

(defn open-all-slixes-and-wait
  ([]
     (open-all-slixes-and-wait false))
  ([startup?]
     (let [sf (get-slix-startup-file)
           sns (if (and startup? (.exists sf))
                 (try
                   (read-string (slurp sf :encoding "UTF-8"))
                   (catch Exception e
                     (log-warning e)
                     nil))
                 (map (fn [[o n [f s]]] [o n]) (find-saved-slixes)))]
       (doseq [[sn name] sns]
         ;; Exclude the slix 'Sevenri' because it's special and is opened
         ;; at the startup time.
         (declare is-slix-sevenri?)
         (when-not (is-slix-sevenri? sn name)
           (when (is-slix-saved? sn name)
             (open-slix-and-wait sn name)))))))

(defmacro open-slix-with-args
  "Return a future oject that opens a slix instance using slix name sn
   and arguments contained in an object args and notifies open events to it.
   Instance name is optional."
  ([args sn]
     `(binding [*open-slix-args* ~args]
        (open-slix ~sn)))
  ([args sn name]
     `(binding [*open-slix-args* ~args]
        (open-slix ~sn ~name))))

(defn alt-open-slix?
  ([]
     (alt-open-slix? *slix*))
  ([slix]
     (let [args (slix-args slix)
           alt-open-kwd (read-prop (get-props) 'slix.argkeyword.alt-open)]
       (and (map? args) (alt-open-kwd args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -save-slix
  "Save slix (meaning, frame) synchronously."
  ([slix io]
     (-save-slix slix io false))
  ([slix io save-on-close?]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [info {:sevenri.event/info-save-on-close save-on-close?}]
         (clear-saved-dynaclass-listeners slix)
         (let [resps (send-event :sevenri.event/slix-saving slix info)
               [res rsn] (get-event-response (get resps (slix-name slix)))]
           (if (= res :sevenri.event/response-exception-occurred)
             ;; save failed 
             (let [eid :sevenri.event/slix-error-save]
               (restore-saved-dynaclass-listeners slix)
               (post-event eid slix (merge info {:reason :sevenri.event/reason-exception-occurred}))
               eid)
             (if (= res :sevenri.event/response-donot-save)
               ;; save canceled
               (let [eid :sevenri.event/slix-save-canceled]
                 (post-event eid slix (merge info {:reason rsn}))
                 eid)
               ;; continue saving
               (let [saved? (atom false)
                     log? (if (= res ::sevenri.event/response-log-xml-encoder-errors)
                            true
                            (if-let [val (get-prop (slix-props slix) 'slix.log.xml-encoder.error)]
                              (read-string val)
                              (read-prop (get-props) 'slix.log.xml-encoder.error)))]
                 (invoke-and-wait slix #(reset! saved? (-save-slix-frame? slix log?)))
                 (let [eid (if @saved?
                             :sevenri.event/slix-saved
                             :sevenri.event/slix-error-save)]
                   (restore-saved-dynaclass-listeners slix)
                   (save-slix-properties slix)
                   (post-event eid slix info)
                   eid)))))))))

(defn save-slix
  "Return a future object that notifies save events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string. Return nil when object is invalid."
  [obj]
  (when-let [slix (get-slix obj)]
    (future (-save-slix slix (get-stdio)))))

(defn save-slix-and-wait
  "Create a future object that notifies save events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [obj]
  (when-let [saver (save-slix obj)]
    @saver))

(defn save-all-slixes-and-wait
  "Wait for all slixes saved."
  []
  (doseq [name (get-slix-names)]
    (save-slix-and-wait name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -can-slix-close?
  [slix close-on-delete? info last-global-event]
  (if (or close-on-delete? ;; cannot deny closing with these conditions
          (= last-global-event :sevenri.event/slixes-closing)
          (= last-global-event :sevenri.event/sevenri-quitting))
    ;;
    (let [info (cond
                (= last-global-event :sevenri.event/slixes-closing)
                  {:sevenri.event/info-close-on-close-slixes true}
                (= last-global-event :sevenri.event/sevenri-quitting)
                  {:sevenri.event/info-close-on-quit-sevenri true}
                :else info)]
      (send-event :sevenri.event/slix-closing slix info)
      [true nil])
    ;;
    (let [resps (send-event :sevenri.event/slix-closing slix info)
          [res rsn] (get-event-response (get resps (slix-name slix)))]
      (if (= res :sevenri.event/response-exception-occurred)
        [false :sevenri.event/reason-exception-occurred]
        (if (= res :sevenri.event/response-donot-close)
          [false rsn]
          [true nil])))))  

(defn- -close-slix
  "Close slix synchronously."
  ([slix io]
     (-close-slix slix io false))
  ([slix io close-on-delete?]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [info {:sevenri.event/info-close-on-delete close-on-delete?}
             lge (get-last-global-event)
             [can-close? reason] (-can-slix-close? slix close-on-delete? info lge)]
         (if-not can-close?
           ;; close canceled or close error by exception
           (let [eid (if (= reason :sevenri.event/reason-exception-occurred)
                       :sevenri.event/slix-error-close
                       :sevenri.event/slix-close-canceled)]
             (post-event eid slix (merge info {:reason reason}))
             eid)
           ;; continue closing
           (let [so (-save-slix slix io true)]
             (if (and (not close-on-delete?) ;; ignore close error when deleting
                      (= so :sevenri.event/slix-error-save))
               ;; close error
               (let [eid :sevenri.event/slix-error-close
                     rsn :sevenri.event/reason-save-error-on-closing]
                 (post-event eid slix (merge info {:reason rsn}))
                 eid)
               ;; closed
               (let [eid :sevenri.event/slix-closed]
                 ;; Unregister the slix. Then dispose its frame and
                 ;; optionally its app-context. Trash instance save
                 ;; directory if it's empty.
                 (-unregister-slix slix)
                 (.dispose (slix-frame slix))
                 (when-let [ac (:app-context (slix-context slix))]
                   (dispose-app-context ac))
                 (when (empty-path? (get-sid-slix-name-path slix))
                   (clean-path? (get-sid-slix-name-path slix) (get-sid-slix-path)))
                 ;;
                 (post-event-to slix eid slix info)
                 (post-event eid slix info)
                 eid))))))))

(defn close-slix
  "Return a future object that notifies close events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string. Return nil when object is invalid."
  [obj]
  (when-let [slix (get-slix obj)]
    (future (-close-slix slix (get-stdio)))))

(defn close-slix-and-wait
  "Create a future object that notifies close events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [obj]
  (when-let [closer (close-slix obj)]
    @closer))

(defn close-all-slixes-and-wait
  "Wait for all slixes closed."
  ([]
     (close-all-slixes-and-wait false))
  ([shutdown?]
     ;; Exclude the slix 'Sevenri' because it's special and is closed at
     ;; the shutdown time.
     (declare is-slix-sevenri?)
     (let [exclude-fn (fn [col] (filter #(not (is-slix-sevenri? %)) col))
           all-slixes (exclude-fn (get-slixes))
           vis-slixes (exclude-fn (map #(get-slix %) (get-z-ordered-frames)))
           unv-slixes (clojure.set/difference (apply hash-set all-slixes)
                                              (apply hash-set vis-slixes))]
       (doseq [slix all-slixes]
         (close-slix-and-wait slix))
       (when shutdown?
         (let [sns (map #(vector (slix-sn %) (slix-name %))
                        (concat unv-slixes (reverse vis-slixes)))
               ssf (get-slix-startup-file)]
           (when (.exists ssf)
             (.delete ssf))
           (spit ssf (pr-str sns) :encoding "UTF-8"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -delete-slix
  [sn name io]
  (binding [*in* (first io) *out* (second io) *err* (last io)]
    (let [info {:sn sn :name name}
          eid :sevenri.event/slix-deleting]
      ;; deleting
      (when-let [slix (get-slix name)]
        ;; Close the running slix forcibly.
        (send-event eid slix)
        (-close-slix slix io true))
      (send-event eid nil info)
      (if (clean-path? (get-sid-slix-name-path sn name) (get-sid-slix-path))
        ;; deleted
        (let [eid :sevenri.event/slix-deleted]
          (post-event eid nil info)
          eid)
        ;; delete failed
        (let [eid :sevenri.event/slixe-error-delete
              rsn :sevenri.event/reason-trash-files-failed]
          (post-event eid nil (merge info {:reason rsn}))
          eid)))))

(defn delete-slix
  "Return a future object that notifies delete events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string. Return nil when object is invalid."
  ([obj]
     (when-let [slix (get-slix obj)]
       (delete-slix (slix-sn slix) (slix-name slix))))
  ([sn name]
     (future (-delete-slix (symbol sn) name (get-stdio)))))

(defn delete-slix-and-wait
  "Create a future object that notifies delete events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  ([obj]
     (when-let [deleter (delete-slix obj)]
       @deleter))
  ([sn name]
     (when-let [deleter (delete-slix sn name)]
       @deleter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- generate-slix-code
  [sn]
  (format (str "(ns ^{:slix true}\n"
               "  slix.%s\n"
               "  (:use [sevenri config core event log props slix ui utils]))\n\n"
               "(defn opened\n"
               "  [event]\n"
               "  (set-slix-visible))\n") sn))

(defn create-slix-file?
  [slix-file sn]
  (try
    (spit slix-file (generate-slix-code sn) :encoding "UTF-8")
    true
    (catch Exception e
      (log-exception e)
      false)))

(defn create-slix
  [sn]
  (let [info {:sn sn}]
    ;; creating
    (send-event :sevenri.event/slix-creating nil info)
    (let [slix-file (get-slix-path (str sn '!clj))
          sn-dir (.getParentFile slix-file)]
      (if (.exists slix-file)
        ;; create failed
        (let [eid :sevenri.event/slix-error-create]
          (send-event eid nil (assoc info :reason :sevenri.event/reason-slix-file-exists))
          eid)
        (let [eid :sevenri.event/slix-created]
          (.mkdirs sn-dir)
          (if (create-slix-file? slix-file sn)
            ;; created
            (let [eid :sevenri.event/slix-created]
              (add-to-slix-sn-cache sn)
              (post-event eid nil info)
              eid)
            ;; create failed
            (let [eid :sevenri.event/slix-error-create
                  rsn :sevenri.event/reason-create-slix-file-failed]
              (send-event eid nil (assoc info :reason rsn))
              eid)))))))

;;;;

(defn purge-slix
  "Purge slix and instance files. Return a purge event id, or nil.
   Cannot purge if instance is running."
  [sn]
  (let [src-sn-file (get-slix-path (str sn '!clj))]
    (when (.exists src-sn-file)
      (let [info {:sn sn}]
        ;; purging
        (send-event ::sevenri.event/slix-purging nil info)
        (let [slixes (filter #(= sn (slix-sn %)) (get-slixes))]
          (if (seq slixes)
            ;; purge failed
            (let [eid :sevenri.event/slix-error-purge
                  rsn :sevenri.event/reason-slix-running]
              (post-event eid nil (assoc info :reason rsn))
              eid)
            ;; continue purging
            (let [trash-sof? (trash-path? src-sn-file)
                  clean-sod? (clean-path? (get-slix-path sn) (get-src-path))
                  clean-dod? (clean-path? (get-sid-slix-path sn) (get-sid-slix-path))]
              (if (and trash-sof? clean-sod? clean-dod?)
                ;; purged
                (let [eid :sevenri.event/slix-purged]
                  (remove-from-slix-sn-cache sn)
                  (clean-path? (get-sid-classes-slix-path sn) (get-sid-classes-path))
                  (post-event eid nil info)
                  eid)
                ;; Purge failed
                (let [eid :sevenri.event/slix-error-purge
                      rsn :sevenri.event/reason-trash-files-failed]
                  (post-event eid nil (assoc info
                                        :reason rsn
                                        :status {:src-sn-file trash-sof?
                                                 :src-slix-dir clean-sod?
                                                 :sid-slix-dir clean-dod?}))
                  eid)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open-slix-sevenri-and-wait
  []
  (let [name (get-sevenri-name)]
    (open-slix-and-wait (.toLowerCase name) name)))

(defn get-slix-sevenri
  []
  (get-slix (get-sevenri-name)))

(defn can-slix-sevenri-close
  ([]
     (when-let [slix (get-slix-sevenri)]
       (get-prop (slix-props slix) 'can.close)))
  ([can?]
     (when-let [slix (get-slix-sevenri)]
       (put-prop (slix-props slix) 'can.close (if can? "true" "false")))))

(defn close-slix-sevenri-and-wait
  []
  (when-let [slix (get-slix-sevenri)]
    (put-prop (slix-props slix) 'can.close "true")
    (close-slix-and-wait slix)))

(defn update-sn-list-of-slix-sevenri
  []
  (when-let [slix-sevenri (get-slix-sevenri)]
    (invoke-slix-public-fn slix-sevenri 'update-sn-list)))

(defn is-slix-sevenri?
  ([obj]
     (if-let [slix (get-slix obj)]
       (is-slix-sevenri? (slix-sn slix) (slix-name slix))
       false))
  ([sn name]
    (if (and (= (symbol sn) 'sevenri)
             (= (str name) (get-sevenri-name)))
      true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-slix-title
  ([title]
     (set-slix-title *slix* title))
  ([slix title]
     (when-let [frame (slix-frame slix)]
       (.setTitle frame (str title))
       (update-sn-list-of-slix-sevenri))))

(defn set-slix-visible
  ([]
     (set-slix-visible *slix* true))
  ([slix]
     (set-slix-visible slix true))
  ([slix visible?]
     (.setVisible (slix-frame slix) visible?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cooperative Path Watch Service
;; Alternative until the watch service of java.nio.file becomes available in
;; JDK7.

(defprotocol PPathWatch
  "Default event types - :create, :update, :remove"
  (add-path-event-listener [watcher path event slix listener]
    "Add the listener owned by the six for the event on the path.")
  ;;
  (remove-path-event-listener [watcher path event slix]
    "Remove the listener owned by the slix for the event on the path.")
  (remove-path-event-listeners [watcher slix] [watcher]
    "Remove all the listeners owned by the slix or slixes that are no longer
     valid.")
  ;;
  (get-watching-paths [watcher] [watcher path-or-slix]
    "Return the map of slix-event-listeners of the specified path or a seq
     of [path event-listeners-owned-by-the-slix] for all paths.")
  ;;
  (notify-path-event [watcher path event slix]
    "Notify the event on the path to listeners. slix is the one causing the
     event."))

;; path-slix-event-listeners structure
;; path-slix-event-listeners :=
;; {pathA {slixA {:create listenerA
;;                :update listenerB
;;                :remove listenerC}
;;         slixB {:update listenerD}
;;  pathB {slixC {:update listenerE}}
;;    :      :           :
;;  pathX {slixX {:remove listenerX}}}

(defn add-path-event-listener*
  [path event slix listener path-slix-event-listeners]
  (if (and (keyword? event) (is-slix? slix)
           (fn? (if (var? listener) (var-get listener) listener)))
    ;; All params are OK.
    (let [path (get-path path)
          slix-event-listeners (get path-slix-event-listeners path)
          event-listeners (get slix-event-listeners slix)]
      (assoc path-slix-event-listeners
        path (assoc slix-event-listeners
               slix (assoc event-listeners event listener))))
    ;; Invalid params. Return path-slix-event-listeners untouched.
    path-slix-event-listeners))

(defn remove-path-event-listener*
  [path event slix path-slix-event-listeners]
  (if (and (keyword? event) (is-slix? slix))
    (let [path (get-path path)
          slix-event-listeners (get path-slix-event-listeners path)
          event-listeners (get slix-event-listeners slix)]
      (if (and slix-event-listeners event-listeners)
        ;; There is slix-event-listeners.
        (let [event-listeners (dissoc event-listeners event)]
          (if (empty? event-listeners)
            ;; No event-listeners for slix
            (let [slix-event-listeners (dissoc slix-event-listeners slix)]
              (if (empty? slix-event-listeners)
                ;; No slix-event-listeners for path
                (dissoc path-slix-event-listeners path)
                ;; There's slix-event-listeners for path still.
                (assoc path-slix-event-listeners path slix-event-listeners)))
            ;; There's event-listeners for slix still.
            (assoc path-slix-event-listeners
              path (assoc slix-event-listeners slix event-listeners))))
        ;; Unknown path or slix
        path-slix-event-listeners))
    ;; Invalid params. Return path-slix-event-listeners untouched.
    path-slix-event-listeners))

(defn remove-path-event-listeners*
  ([slix path-slix-event-listeners]
     (if (is-slix? slix)
       ;; Remove all event-listeners owned by the slix for all paths.
       (reduce (fn [psel path]
                 (let [slix-event-listeners (dissoc (get psel path) slix)]
                   (if (empty? slix-event-listeners)
                     (dissoc psel path)
                     (assoc psel path slix-event-listeners))))
               path-slix-event-listeners
               (keys path-slix-event-listeners))
       ;; Invalid param. Return path-slix-event-listeners untouched.
       path-slix-event-listeners))
  ([path-slix-event-listeners]
     ;; Remove all invalid slix and its event-listeners.
     (reduce (fn [psel slix]
               (if (get-slix slix)
                 psel
                 (remove-path-event-listeners* slix psel)))
             path-slix-event-listeners
             (distinct (apply concat (map keys (vals path-slix-event-listeners)))))))

(defn get-watching-paths*
  [path-or-slix path-slix-event-listeners]
  (if (is-slix? path-or-slix)
    (let [slix path-or-slix]
      ;; Return a seq of [path event-listeners-owned-by-the-slix] for all paths.
      (seq (remove nil? (map #(when-let [event-listeners (get (get path-slix-event-listeners %) slix)]
                                [% event-listeners])
                             (keys path-slix-event-listeners)))))
    (let [path (get-path path-or-slix)]
      ;; Return the map of slix-evnt-listeners of the path.
      (get path-slix-event-listeners path))))

(defn notify-path-event*
  [path event src-slix ref-path-slix-event-listeners]
  (when (and (keyword? event) (is-slix? src-slix))
    (let [path (get-path path)
          cmpp (if (.exists path) ;; do canonical file comparison for existing file
                 (fn [p1 p2] (.equals (.getCanonicalFile p1) (.getCanonicalFile p2)))
                 (fn [p1 p2] (.equals (.getAbsoluteFile p1) (.getAbsoluteFile p2))))
          evnt {:path path :slix src-slix :event event}]
      (doseq [p (keys @ref-path-slix-event-listeners)]
        (when (cmpp p path)
          ;; There are listeners on the path.
          (doseq [slix-event-listeners (get @ref-path-slix-event-listeners p)]
            (let [[slix event-listeners] slix-event-listeners]
              (if (get-slix slix)
                (when-let [listener (get event-listeners event)]
                  (try
                    (binding [*slix* slix]
                      (listener evnt))
                    (catch Exception e
                      (log-warning "notify-path-event* exception on event:" evnt
                                   "\n" (get-stack-trace-print-lines e))
                      ;; Get rid of broken listener.
                      (reset! ref-path-slix-event-listeners
                              (remove-path-event-listener* p event slix @ref-path-slix-event-listeners)))))
                ;; Invalid slix. Get rid of it and its listeners.
                (reset! ref-path-slix-event-listeners
                        (remove-path-event-listeners* slix @ref-path-slix-event-listeners))))))))))

;;;;

(defn get-path-watcher
  []
  *path-watcher*)

(defn create-path-watcher*
  []
  (let [psel (atom {})] ;; psel := path-slix-event-listeners
    (reify PPathWatch
      (add-path-event-listener [_ path event slix listener]
        (reset! psel (add-path-event-listener* path event slix listener @psel)))
      ;;
      (remove-path-event-listener [_ path event slix]
        (reset! psel (remove-path-event-listener* path event slix @psel)))
      (remove-path-event-listeners [_ slix]
        (reset! psel (remove-path-event-listeners* slix @psel)))
      (remove-path-event-listeners [_]
        (reset! psel (remove-path-event-listeners* @psel)))
      ;;
      (get-watching-paths [_] @psel)
      (get-watching-paths [_ path-or-slix] (get-watching-paths* path-or-slix @psel))
      ;;
      (notify-path-event [_ path event slix] (notify-path-event* path event slix psel))
      ;;
      (toString [this] (str "PPathWatch [" (get-watching-paths this) "]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

(defn get-xref-slix
  []
  @*xref-slix*)

(defn get-xref-key
  []
  @*xref-key*)

(defn get-xref-val
  []
  @*xref-val*)

(defmulti xref-with
  (fn [obj]
    (cond
     (is-slix? obj) :slix
     (keyword? obj) :key
     :else :val)))

(defmethod xref-with :slix
  [slix]
  (get (get-xref-slix) slix))

(defmethod xref-with :key
  [key]
  (key (get-xref-key)))

(defmethod xref-with :val
  [val]
  (get (get-xref-val) val))

(defn remove-from-xref
  ([slix]
     (when (is-slix? slix)
       (doseq [[key _] (xref-with slix)]
         (remove-from-xref slix key))))
  ([slix key]
     (when (and (is-slix? slix) (keyword? key))
       (dosync
        (let [old-ovs (xref-with key)]
          (doseq [[_ val] (filter (fn [[o v]] (identical? o slix)) old-ovs)]
            (let [old-oks (xref-with val)
                  new-oks (reduce (fn [m [o k]] (if (identical? o slix)
                                                  m
                                                  (assoc m o k)))
                                  {} old-oks)]
              (ref-set *xref-val* (if (empty? new-oks)
                                    (dissoc (get-xref-val) val)
                                    (assoc (get-xref-val) val new-oks)))))
          (let [new-ovs (reduce (fn [m [o v]] (if (identical? o slix)
                                                m
                                                (assoc m o v)))
                                {} old-ovs)]
            (ref-set *xref-key* (if (empty? new-ovs)
                                  (dissoc (get-xref-key) key)
                                  (assoc (get-xref-key) key new-ovs))))
          (let [new-kvs (reduce (fn [m [k v]] (if (= k key)
                                                m
                                                (assoc m k v)))
                                {} (xref-with slix))]
            (ref-set *xref-slix* (if (empty? new-kvs)
                                    (dissoc (get-xref-slix) slix)
                                    (assoc (get-xref-slix) slix new-kvs)))))))))

(defn add-to-xref
  [slix key val]
  (declare get-slix)
  (when (and (is-slix? slix) (get-slix slix)) (keyword? key))
    (remove-from-xref slix key)
    (dosync
     (ref-set *xref-slix* (assoc (get-xref-slix)
                            slix
                            (assoc (xref-with slix) key val)))
     (ref-set *xref-key* (assoc (get-xref-key)
                           key
                           (assoc (xref-with key) slix val)))
     (ref-set *xref-val* (assoc (get-xref-val)
                           val
                           (assoc (xref-with val) slix key)))))

;;;;

(defn put-slix-prop
  ([key val]
     (put-slix-prop *slix* key val))
  ([slix key val]
     (let [old-prop (:prop_ (slix-context slix))
           new-prop (assoc @old-prop key val)]
       (dosync (ref-set old-prop new-prop))
       new-prop))
  ([slix key val & kvs]
    (let [new-prop (put-slix-prop slix key val)]
      (if (seq kvs)
        (recur slix (first kvs) (second kvs) (nnext kvs))
        new-prop))))

(defn get-slix-prop
  "Returns the value mapped to key of the default or given slix property,
   or not-found or nil if key not present."
  ([key]
     (get-slix-prop *slix* key nil))
  ([slix key]
     (get-slix-prop slix key nil))
  ([slix key not-found]
     (get (deref (:prop_ (slix-context slix))) key not-found)))

(defn remove-slix-prop
  ([key]
     (remove-slix-prop *slix* key))
  ([slix key]
     (let [old-prop (:prop_ (slix-context slix))
           new-prop (dissoc @old-prop key)]
       (dosync (ref-set old-prop new-prop))
       new-prop))
  ([slix key & ks]
     (let [new-prop (remove-slix-prop slix key)]
       (if (seq ks)
         (recur slix (first ks) (next ks))
         new-prop))))

(defn clear-slix-prop
  ([]
     (clear-slix-prop *slix*))
  ([slix]
     (dosync (ref-set (:prop_ (slix-context slix)) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn- -acquire-base-class-loader?
  []
  (redef! *base-class-loader* (.getContextClassLoader (Thread/currentThread)))
  true)

(defn- -acquire-system-event-queue?
  []
  (redef! *system-event-queue* (.getSystemEventQueue (java.awt.Toolkit/getDefaultToolkit)))
  true)

(defn- -setup-slix-dirs?
  []
  (with-make-path
    (get-sid-slix-path)
    (get-sid-trash-path))
  true)

(defn- -cache-slix-sns?
  []
  (reset! *slix-sn-cache* (apply conj @*slix-sn-cache* (find-all-slix-sn)))
  true)

(defn- -register-exception-listeners?
  []
  (doseq [sn (get-all-slix-sn)]
    (when-let [nm (:exception-listener (if (find-ns (get-slix-ns sn))
                                         (get-slix-sn-meta sn)
                                         (meta sn)))]
      (if (symbol? nm)
        (register-exception-listener sn nm)
        (when (and (seq nm)
                   (symbol? (last nm)))
          (register-exception-listener sn (last nm))))))
  true)

(defn- -aot-compile-slix-sevenri?
  "At startup time Sevenri aot-compiles slix.sevenri only (and if
   slix.sevenri.aot exists). AOT-compiling other slixes should be done by
   slix.sevenri later."
  []
  (without-make-path
   (if (.exists (get-src-slix-path 'sevenri 'aot!clj))
     (aot-compile? 'sevenri 'aot false)
     true)))

(defn- -setup-path-watcher?
  []
  (redef! *path-watcher* (create-path-watcher*))
  true)

;;;;

(defn- -setup-mac-dependents
  []
  (add-mac-about-handler (fn []
                           (if-let [slix (get-slix "Sevenri.about")]
                             (.toFront (slix-frame slix))
                             (open-slix 'sevenri.about)))))

(defn- -setup-platform-dependents?
  []
  (cond
   (is-mac?) (-setup-mac-dependents)
   :else nil)
  true)

;;;;

(defn startup-slix?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -acquire-base-class-loader?
          -acquire-system-event-queue?
          -setup-slix-dirs?
          -cache-slix-sns?
          -register-exception-listeners?
          -aot-compile-slix-sevenri?
          -setup-path-watcher?
          -setup-platform-dependents?)))

(defn shutdown-slix?
  []
  (apply while-each-true?
         nil))
