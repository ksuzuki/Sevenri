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

(ns ^{:doc "slix, Sevenri library complex"}
  sevenri.slix
  (:use [sevenri config core defs event log jvm os props refs ui utils])
  (:import (java.awt.event KeyAdapter KeyEvent)
           (java.beans ExceptionListener XMLEncoder XMLDecoder)
           (java.io BufferedOutputStream BufferedInputStream
                    File FileFilter FileInputStream FileOutputStream
                    InputStreamReader PushbackReader)
           (java.net URL URLClassLoader)
           (javax.swing JFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *slix* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro slix-fn
  [name]
  (let [fn-name# (symbol (str 'slix- name))
        slix-keyword# (keyword name)]
    `(defn ~fn-name#
       ([] (~slix-keyword# ~'*slix*))
       ([~'slix] (~slix-keyword# ~'slix)))))

;; :id - instance id
(slix-fn id)
;; :sn - slix name
(slix-fn sn)
;; :name - slix instance name
(slix-fn name)
;; :cl - per-slix class loader
(slix-fn cl)
;; :context - {:prop (ref {})}, plus {:app-context app-context} optionally
(slix-fn context)
;; :frame - associated JFrame
(slix-fn frame)
;; :args - arguments
(slix-fn args)

;;;;

(defn is-slix?
  [object]
  (and (map? object)
       (every? identity [(slix-id object) (slix-sn object) (slix-name object)])))

(defn get-slix-ns
  ([sn]
     (symbol (str 'slix \. sn)))
  ([sn sym]
     (get-slix-ns (str sn \. sym)))
  ([sn sym & syms]
     (apply get-slix-ns sn (str sym \. (first syms)) (rest syms))))

(defn create-slix-context
  ([]
     {:prop_ (ref {})})
  ([app-context]
     {:prop_ (ref {}) :app-context app-context}))

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
  (fn [object]
    (cond
     (is-slix? object) :slix
     (keyword? object) :key
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
  (when (and (is-slix? slix)
             (declare get-slix)
             (identical? slix (get-slix slix))
             (keyword? key))
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
                           (assoc (xref-with val) slix key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-slixes
  ([]
     (vals @*slixes*))
  ([sn]
     (when (or (symbol? sn) (string? sn))
       (seq (filter #(= (symbol sn) (slix-sn %)) (get-slixes))))))

(defmulti get-slix
  (fn [object]
    (cond
     (or (string? object) (symbol? object)) :name
     (is-slix? object) :slix
     (instance? JFrame object) :frame
     :else :default)))

(defmethod get-slix :name
  [object]
  (get-prop_ *slixes* (str object)))

(defmethod get-slix :slix
  [object]
  (when (identical? object (get-prop_ *slixes* (str (slix-name object))))
    object))

(defmethod get-slix :frame
  [object]
  (first (filter #(identical? object (slix-frame %)) (get-slixes))))

(defmethod get-slix :default
  [object]
  nil)

(defn get-slix-names
  []
  (keys @*slixes*))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn get-sid-slix-frame-file
  ([]
     (when *slix*
       (get-sid-slix-frame-file *slix*)))
  ([slix]
     (get-sid-slix-frame-file (slix-sn slix) (slix-name slix)))
  ([sn name]
     (get-sid-slix-frame-file sn name (get-config 'sid.slix.save.frame-file-name)))
  ([sn name frame-file-name]
     (let [dir (with-make-path (get-sid-slix-name-path sn name))]
       (get-path dir frame-file-name))))

(defn get-sid-slix-state-file
  ([]
     (when *slix*
       (get-sid-slix-state-file *slix*)))
  ([slix]
     (get-sid-slix-state-file (slix-sn slix) (slix-name slix)))
  ([sn name]
     (get-sid-slix-state-file sn name (get-config 'sid.slix.save.state-file-name)))
  ([sn name state-file-name]
     (let [dir (with-make-path (get-sid-slix-name-path sn name))]
       (get-path dir state-file-name))))

(defn get-slix-file-bundle
  "Return [frame-file state-file] or nil"
  ([]
     (when *slix*
       (get-slix-file-bundle *slix*)))
  ([slix]
     (get-slix-file-bundle (slix-sn slix) (slix-name slix)))
  ([sn name]
     [(get-sid-slix-frame-file sn name) (get-sid-slix-state-file sn name)]))

(defn is-slix-saved
  "Return [frame-file state-file/nil] or nil."
  ([]
     (when *slix*
       (is-slix-saved *slix*)))
  ([slix]
     (is-slix-saved (slix-sn slix) (slix-name slix)))
  ([sn name]
     (let [[f s] (get-slix-file-bundle sn name)]
       (when (.exists f)
         [f (when (.exists s) s)]))))

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
       (let [[f s] (get-slix-file-bundle sn name)]
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
      (let [[f s] (get-slix-file-bundle (slix-sn slix) (slix-name slix))]
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

(defn register-slix
  ([slix]
     (register-slix slix (slix-name slix)))
  ([slix name]
     (dosync
      (ref-set *slixes* (assoc @*slixes* (str name) slix)))))

(defn unregister-slix
  ([slix]
     (unregister-slix slix (slix-name slix)))
  ([slix name]
     (remove-from-xref slix)
     (dosync
      (ref-set *slixes* (dissoc @*slixes* (str name))))))

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

(defn is-singleton-slix?
  [object]
  (let [sn (if (is-slix? object)
             (slix-sn object)
             (symbol (str object)))
        fsn (filter #(= sn %) @*slix-sn-cache*)]
    (if (seq fsn)
      (true? (:singleton (get-slix-sn-meta (first fsn))))
      false)))

(defn- -create-initial-frame
  [slix]
  (let [f (JFrame.)
        n (slix-name slix)
        [w h] (read-prop (get-properties) 'slix.frame.size)]
    (doto f
      (.setLocationByPlatform *frame-location-by-platform*)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setTitle (str n))
      (.setSize w h))
    f))

(defn- -abort-open-slix
  ([slix]
     (-abort-open-slix slix
                        :sevenri.event/slix-error-open
                        :sevenri.event/reason-exception-occurred))
  ([slix eid reason]
     (-abort-open-slix slix eid reason true))
  ([slix eid reason post-event?]
     (-slix-is-opening (slix-name slix) false)
     (when (identical? (get-slix (slix-name slix)) slix)
       (unregister-slix slix))
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
               (let [saved? (if (is-slix-saved slix) true false)]
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
     ;; Install the default listeners.
     (when-not saved?
       (doto frame
         (add-default-window-listener)
         (add-default-key-listener)))
     (let [slix (assoc slix :frame frame)
           eid (if saved?
                 :sevenri.event/slix-frame-loaded
                 :sevenri.event/slix-frame-created)]
       ;; frame created or loaded
       (-send-event-and-continue-unless
        nil ;; ignore any response
        slix eid send-creation-event
        (register-slix slix)
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
     (if-let [app-context (create-app-context (slix-name slix) (slix-cl slix))]
       ;; EDT per slix
       (-get-context-and-start-slix-creation slix (create-slix-context app-context))
       ;; sharing the same, main EDT
       (-get-context-and-start-slix-creation slix (create-slix-context))))
  ([slix context]
     (let [slix (assoc slix :context context)]
       (future
         (try
           (-open-slix slix (get-stdio))
           (catch Exception e
             (log-exception e)))))))

;;;;

(def -open-slix-args- nil)

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
             slix {:id (gensym 'id) :sn sn :name name :cl cl :args -open-slix-args-}]
         (-get-context-and-start-slix-creation slix))
       (future
         (when-let [projman (get-project-manager)]
           (build-and-run projman (get-slix-ns sn) sn name -open-slix-args-)
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
           (when (is-slix-saved sn name)
             (open-slix-and-wait sn name)))))))

(defmacro open-slix-with-args
  "Return a future oject that opens a slix instance using slix name sn
   and arguments contained in an object args and notifies open events to it.
   Instance name is optional."
  ([args sn]
     `(binding [-open-slix-args- ~args]
        (open-slix ~sn)))
  ([args sn name]
     `(binding [-open-slix-args- ~args]
        (open-slix ~sn ~name))))

(defn alt-open-slix?
  ([]
     (alt-open-slix? *slix*))
  ([slix]
     (let [args (slix-args slix)
           alt-open-kwd (read-prop (get-properties) 'slix.argkeyword.alt-open)]
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
                            *log-xml-encoder-errors*)]
                 (invoke-and-wait slix #(reset! saved? (-save-slix-frame? slix log?)))
                 (let [eid (if @saved?
                             :sevenri.event/slix-saved
                             :sevenri.event/slix-error-save)]
                   (restore-saved-dynaclass-listeners slix)
                   (post-event eid slix info)
                   eid)))))))))

(defn save-slix
  "Return a future object that notifies save events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string. Return nil when object is invalid."
  [object]
  (when-let [slix (get-slix object)]
    (future (-save-slix slix (get-stdio)))))

(defn save-slix-and-wait
  "Create a future object that notifies save events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [object]
  (when-let [saver (save-slix object)]
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
                 (unregister-slix slix)
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
  [object]
  (when-let [slix (get-slix object)]
    (future (-close-slix slix (get-stdio)))))

(defn close-slix-and-wait
  "Create a future object that notifies close events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [object]
  (when-let [closer (close-slix object)]
    @closer))

(defn close-all-slixes-and-wait
  "Wait for all slixes closed."
  ([]
     (close-all-slixes-and-wait false))
  ([shutdown?]
     ;; Exclude the slix 'Sevenri' because it's special and is closed at
     ;; the shutdown time.
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
  ([object]
     (when-let [slix (get-slix object)]
       (delete-slix (slix-sn slix) (slix-name slix))))
  ([sn name]
     (future (-delete-slix (symbol sn) name (get-stdio)))))

(defn delete-slix-and-wait
  "Create a future object that notifies delete events to slix instance
   specified by object, which can be slix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  ([object]
     (when-let [deleter (delete-slix object)]
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

(defn close-slix-sevenri-and-wait
  []
  (close-slix-and-wait (get-sevenri-name)))

(defn get-slix-sevenri
  []
  (get-slix (get-sevenri-name)))

(defn update-slix-sevenri-lists
  []
  (let [slix-sevenri (get-slix-sevenri)]
    (when-let [update-lists-fn (:update-lists-fn (xref-with slix-sevenri))]
      (when (fn? (var-get update-lists-fn))
        (invoke-later slix-sevenri update-lists-fn)))))

(defn is-slix-sevenri?
  ([object]
     (if-let [slix (get-slix object)]
       (is-slix-sevenri? (slix-sn slix) (slix-name slix))
       false))
  ([sn name]
    (if (and (= (symbol sn) 'sevenri)
             (= (str name) (get-sevenri-name)))
      true
      false)))

(defn can-slix-sevenri-close?
  []
  *slix-sevenri-can-close*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-slix-title
  ([title]
     (set-slix-title *slix* title))
  ([slix title]
     (when-let [frame (slix-frame slix)]
       (.setTitle frame (str title))
       (update-slix-sevenri-lists))))

(defn set-slix-visible
  ([]
     (set-slix-visible *slix* true))
  ([slix]
     (set-slix-visible slix true))
  ([slix visible?]
     (.setVisible (slix-frame slix) visible?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

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
  (when-not *base-class-loader*
    (reset-base-class-loader (.getContextClassLoader (Thread/currentThread))))
  true)

(defn- -acquire-system-event-queue?
  []
  (when-not *system-event-queue*
    (reset-system-event-queue (.getSystemEventQueue (java.awt.Toolkit/getDefaultToolkit))))
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

;;;;

(defn- -setup-mac-dependents
  []
  (add-mac-about-handler (fn [] (open-slix 'sevenri.about))))

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
          -setup-platform-dependents?)))

(defn shutdown-slix?
  []
  (apply while-each-true?
         nil))
