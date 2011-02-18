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

;; oplix - Openar library complex

(ns openar.oplix
  (:use [openar config core defs event log jvm os refs ui utils])
  (:import (java.awt.event KeyAdapter KeyEvent)
           (java.beans ExceptionListener XMLEncoder XMLDecoder)
           (java.io BufferedOutputStream BufferedInputStream
                    File FileFilter FileInputStream FileOutputStream
                    InputStreamReader PushbackReader)
           (java.net URL URLClassLoader)
           (javax.swing JFrame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *oplix* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro oplix-fn
  [name]
  (let [fn-name# (symbol (str 'oplix- name))
        oplix-keyword# (keyword name)]
    `(defn ~fn-name#
       ([] (~oplix-keyword# ~'*oplix*))
       ([~'oplix] (~oplix-keyword# ~'oplix)))))

;; :oid - oplix id
(oplix-fn oid)
;; :on - oplix name
(oplix-fn on)
;; :name - oplix instance name
(oplix-fn name)
;; :cl - per-oplix class loader
(oplix-fn cl)
;; :context - {:prop (ref {})}, plus {:app-context app-context} optionally
(oplix-fn context)
;; :frame - associated JFrame
(oplix-fn frame)
;; :args - arguments
(oplix-fn args)

(defn is-oplix?
  [object]
  (and (map? object)
       (every? identity [(oplix-oid object) (oplix-on object) (oplix-name object)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-oplix-context
  ([]
     {:prop (ref {})})
  ([app-context]
     {:prop (ref {}) :app-context app-context}))

(defn put-oplix-prop
  ([key val]
     (put-oplix-prop *oplix* key val))
  ([oplix key val]
     (let [old-prop (:prop (oplix-context oplix))
           new-prop (assoc @old-prop key val)]
       (dosync (ref-set old-prop new-prop))
       new-prop))
  ([oplix key val & kvs]
    (let [new-prop (put-oplix-prop oplix key val)]
      (if (seq kvs)
        (recur oplix (first kvs) (second kvs) (nnext kvs))
        new-prop))))

(defn get-oplix-prop
  "Returns the value mapped to key of the default or given oplix property,
   or not-found or nil if key not present."
  ([key]
     (get-oplix-prop *oplix* key nil))
  ([oplix key]
     (get-oplix-prop oplix key nil))
  ([oplix key not-found]
     (get (deref (:prop (oplix-context oplix))) key not-found)))

(defn remove-oplix-prop
  ([key]
     (remove-oplix-prop *oplix* key))
  ([oplix key]
     (let [old-prop (:prop (oplix-context oplix))
           new-prop (dissoc @old-prop key)]
       (dosync (ref-set old-prop new-prop))
       new-prop))
  ([oplix key & ks]
     (let [new-prop (remove-oplix-prop oplix key)]
       (if (seq ks)
         (recur oplix (first ks) (next ks))
         new-prop))))

(defn clear-oplix-prop
  ([]
     (clear-oplix-prop *oplix*))
  ([oplix]
     (dosync (ref-set (:prop (oplix-context oplix)) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-xref-oplix
  []
  @*xref-oplix*)

(defn get-xref-key
  []
  @*xref-key*)

(defn get-xref-val
  []
  @*xref-val*)

(defmulti xref-with
  (fn [object]
    (cond
     (is-oplix? object) :oplix
     (keyword? object) :key
     :else :val)))

(defmethod xref-with :oplix
  [oplix]
  (get (get-xref-oplix) oplix))

(defmethod xref-with :key
  [key]
  (key (get-xref-key)))

(defmethod xref-with :val
  [val]
  (get (get-xref-val) val))

(defn remove-from-xref
  ([oplix]
     (when (is-oplix? oplix)
       (doseq [[key _] (xref-with oplix)]
         (remove-from-xref oplix key))))
  ([oplix key]
     (when (and (is-oplix? oplix) (keyword? key))
       (dosync
        (let [old-ovs (xref-with key)]
          (doseq [[_ val] (filter (fn [[o v]] (identical? o oplix)) old-ovs)]
            (let [old-oks (xref-with val)
                  new-oks (reduce (fn [m [o k]] (if (identical? o oplix)
                                                  m
                                                  (assoc m o k)))
                                  {} old-oks)]
              (ref-set *xref-val* (if (empty? new-oks)
                                    (dissoc (get-xref-val) val)
                                    (assoc (get-xref-val) val new-oks)))))
          (let [new-ovs (reduce (fn [m [o v]] (if (identical? o oplix)
                                                m
                                                (assoc m o v)))
                                {} old-ovs)]
            (ref-set *xref-key* (if (empty? new-ovs)
                                  (dissoc (get-xref-key) key)
                                  (assoc (get-xref-key) key new-ovs))))
          (let [new-kvs (reduce (fn [m [k v]] (if (= k key)
                                                m
                                                (assoc m k v)))
                                {} (xref-with oplix))]
            (ref-set *xref-oplix* (if (empty? new-kvs)
                                    (dissoc (get-xref-oplix) oplix)
                                    (assoc (get-xref-oplix) oplix new-kvs)))))))))

(defn add-to-xref
  [oplix key val]
  (when (and (is-oplix? oplix)
             (declare get-oplix)
             (identical? oplix (get-oplix oplix))
             (keyword? key))
    (remove-from-xref oplix key)
    (dosync
     (ref-set *xref-oplix* (assoc (get-xref-oplix)
                             oplix
                             (assoc (xref-with oplix) key val)))
     (ref-set *xref-key* (assoc (get-xref-key)
                           key
                           (assoc (xref-with key) oplix val)))
     (ref-set *xref-val* (assoc (get-xref-val)
                           val
                           (assoc (xref-with val) oplix key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-oplixes
  ([]
     (vals @*oplixes*))
  ([on]
     (when (or (symbol? on) (string? on))
       (seq (filter #(= (symbol on) (oplix-on %)) (get-oplixes))))))

(defmulti get-oplix
  (fn [object]
    (cond
     (or (string? object) (symbol? object)) :name
     (is-oplix? object) :oplix
     (instance? JFrame object) :frame
     :else :default)))

(defmethod get-oplix :name
  [object]
  (get-prop *oplixes* (str object)))

(defmethod get-oplix :oplix
  [object]
  (when (identical? object (get-prop *oplixes* (str (oplix-name object))))
    object))

(defmethod get-oplix :frame
  [object]
  (first (filter #(identical? object (oplix-frame %)) (get-oplixes))))

(defmethod get-oplix :default
  [object]
  nil)

(defn get-oplix-names
  []
  (keys @*oplixes*))

(defn add-to-oplix-on-cache
  [on]
  (reset! *oplix-on-cache* (conj @*oplix-on-cache* on)))

(defn remove-from-oplix-on-cache
  [on]
  (reset! *oplix-on-cache* (disj @*oplix-on-cache* on)))

(defn get-all-oplix-on
  []
  @*oplix-on-cache*)

(defn get-oplix-fqns
  ([on]
     (symbol (str (get-default :on :oplix) \. on)))
  ([on pfx]
     (get-oplix-fqns (str on \. pfx)))
  ([on pfx & pfxs]
     (apply get-oplix-fqns on (str pfx \. (first pfxs)) (rest pfxs))))

(defn get-oplix-on-meta
  [on]
  (when-let [ons (find-ns (get-oplix-fqns on))]
    (meta ons)))

(defn get-library-oplix-fqns
  [on & pfxs]
  (symbol (str (get-default :on :library) \. (apply get-oplix-fqns on pfxs))))

(defn get-oplix-fn
  [on fnsym]
  (ns-resolve (get-oplix-fqns on) fnsym))

(defn get-src-oplix-dir
  ([]
     (get-dir (get-src-dir) (get-default :src :oplix :dir-name)))
  ([on & pfxs]
     (get-dir (get-src-dir) (nssym2path (apply get-oplix-fqns on pfxs)))))

(defmacro get-oplix-dir
  ([]
     `(get-src-oplix-dir))
  ([on & pfxs]
     `(get-src-oplix-dir ~on ~@pfxs)))

(defn get-src-oplix-file
  [on & pfxs]
  (File. (get-src-dir) (str (nssym2path (apply get-oplix-fqns on pfxs)) ".clj")))

(defmacro get-oplix-file
  [on & pfxs]
  `(get-src-oplix-file ~on ~@pfxs))

(defn get-src-library-oplix-dir
  ([]
     (get-src-library-dir (get-default :src :library :oplix :dir-name)))
  ([on & pfxs]
     (get-dir (get-src-library-dir) (nssym2path (apply get-oplix-fqns on pfxs)))))

(defmacro get-library-oplix-dir
  ([]
     `(get-src-library-oplix-dir))
  ([on & pfxs]
     `(get-src-library-oplix-dir ~on ~@pfxs)))

(defn get-src-library-oplix-file
  [on name & pfxs]
  (if (seq pfxs)
    (File. (get-src-library-dir (nssym2path (apply get-oplix-fqns on name (butlast pfxs))))
           (str (nssym2path (last pfxs)) ".clj"))
    (File. (get-src-library-dir (nssym2path (get-oplix-fqns on)))
           (str (nssym2path name) ".clj"))))

(defmacro get-library-oplix-file
  [on name & pfxs]
  `(get-src-library-oplix-file ~on ~name ~@pfxs))

(defn get-dop-classes-oplix-dir
  [on & pfxs]
  (get-dir (get-dop-classes-dir) (nssym2path (apply get-oplix-fqns on pfxs))))

(defn get-dop-oplix-dir
  ([]
     (get-dop-dir (get-default :dop :oplix :dir-name)))
  ([on & pfxs]
     (get-dir (get-dop-root-dir) (apply get-oplix-fqns on pfxs))))

(defn get-dop-oplix-file
  [on & pfxs]
  (File. (get-dop-root-dir) (str (nssym2path (apply get-oplix-fqns on pfxs)) ".clj")))

(defn get-dop-oplix-save-dir
  [on]
  (get-dop-oplix-dir on (get-default :dop :oplix :save :dir-name)))

(defn get-dop-oplix-name-dir
  ([oplix]
     (get-dop-oplix-name-dir (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     (get-dir (get-dop-oplix-save-dir on) name)))

(defn get-oplix-startup-file
  []
  (File. (get-dop-oplix-dir) (str (get-default :dop :oplix :startup :file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-all-oplix-on
  "Parse clj files (except scratch) and return oplix namespaces without
   'oplix.' prefix."
  []
  (let [oplix (get-default :src :oplix :dir-name)
        rpclj (re-pattern (str "/" oplix "/.*\\.clj$"))
        rpsct (re-pattern (str ".*/" (get-default :src :oplix :scratch-file-name) "$"))
        cljfs (filter #(not (re-matches rpsct (str %)))
                      (find-files #(re-find rpclj (str %)) (get-oplix-dir)))]
    (filter identity
            (map (fn [f]
                   (with-open [rdr (PushbackReader. (InputStreamReader. (FileInputStream. f) "UTF-8"))]
                     (when-let [obj (try
                                      (read rdr)
                                      (catch Exception e
                                        (log-severe "find-all-oplix-on failed:" f)
                                        nil))]
                       (let [sym1 (first obj)
                             sym2 (second obj)
                             str2 (str sym2)]
                         (when (and (= 'ns sym1)
                                    (< 6 (count str2)) ;; 6 := 'oplix.'
                                    (= "oplix." (subs str2 0 6))
                                    (true? (:oplix (meta sym2))))
                           (with-meta (symbol (subs str2 6)) (meta sym2)))))))
                 cljfs))))

(defn get-dop-oplix-frame-file
  ([]
     (when *oplix*
       (get-dop-oplix-frame-file *oplix*)))
  ([oplix]
     (get-dop-oplix-frame-file (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     (get-dop-oplix-frame-file on name (get-default :dop :oplix :save :frame-file-name)))
  ([on name frame-file-name]
     (File. (get-dop-oplix-name-dir on name) (str frame-file-name))))

(defn get-dop-oplix-state-file
  ([]
     (when *oplix*
       (get-dop-oplix-state-file *oplix*)))
  ([oplix]
     (get-dop-oplix-state-file (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     (get-dop-oplix-state-file on name (get-default :dop :oplix :save :state-file-name)))
  ([on name state-file-name]
     (File. (get-dop-oplix-name-dir on name) (str state-file-name))))

(defn get-oplix-file-bundle
  "Return [frame-file state-file] or nil"
  ([]
     (when *oplix*
       (get-oplix-file-bundle *oplix*)))
  ([oplix]
     (get-oplix-file-bundle (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     [(get-dop-oplix-frame-file on name) (get-dop-oplix-state-file on name)]))

(defn is-oplix-saved
  "Return [frame-file state-file/nil] or nil."
  ([]
     (when *oplix*
       (is-oplix-saved *oplix*)))
  ([oplix]
     (is-oplix-saved (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     (let [[f s] (get-oplix-file-bundle on name)]
       (when (.exists f)
         [f (when (.exists s) s)]))))

(defn get-saved-oplix-names
  "Return a seq of names or nil"
  [on]
  (when-let [od (get-dop-oplix-save-dir on)]
    (let [ff (proxy [FileFilter] []
               (accept [f] (.isDirectory f)))]
      (seq (map #(.getName %) (.listFiles od ff))))))

(defn find-saved-oplixes
  "Return a seq of [on name [frame-file state-file/nil]] or nil."
  ([]
     (let [;; Find any files under dop/oplix, convert them in string, and then sort them.
           afps (sort (map str (find-files #(.isFile %) (get-dop-oplix-dir))))
           ;; Remove up to dop/oplix and go to the next stage.
           _sv_ (get-default :dop :oplix :save :dir-name)
           rptn (re-pattern (str "^" (get-dop-oplix-dir) "/(.*/" _sv_ "/.*)$"))]
       (find-saved-oplixes (filter identity (map #(second (re-find rptn %)) afps))
                          (str (get-default :dop :oplix :save :frame-file-name))
                          (str (get-default :dop :oplix :save :state-file-name)))))
  ([onfiles ffname sfname]
     ;; Return a lazy-seq that consumes on-name-files and return [on name [f s/nil]]s.
     (lazy-seq
      (when (seq onfiles)
        (let [on_name (.getParentFile (File. (first onfiles))) ;; on/_save_/name
              on (.getParent (.getParentFile on_name))
              nm (.getName on_name)
              create-item (fn [fs]
                            (let [fsv (vec (map #(File. (File. (get-dop-oplix-dir) (str on_name)) %) fs))]
                              [(symbol (path2nssym on)) (str nm) (if (< (count fsv) 2) (conj fsv nil) fsv)]))]
          (loop [onfiles onfiles
                 file-bundle nil]
            (if (seq onfiles)
              ;; There are onfiles.
              (let [onfile (File. (first onfiles))
                    ofname (.getName onfile)]
                (if (= on_name (.getParentFile onfile))
                  ;; Same on/_save_/name. Add to file-bundle and loop.
                  (cond
                   (= ofname ffname) (recur (rest onfiles) (cons ffname file-bundle))
                   (= ofname sfname) (recur (rest onfiles) (if (= (first file-bundle) ffname)
                                                             (concat (list ffname sfname) (rest file-bundle))
                                                             (cons sfname file-bundle)))
                   ;; Currently ignore any files other than frame.xml or state.clj.
                   ;; If we want to list others too, do this:
                   ;;   :else (recur (rest onfiles) (concat file-bundle (list ofname))))
                   :else (recur (rest onfiles) file-bundle))
                  ;; Different on/_save_/name.
                  (if (seq file-bundle)
                    ;; There is file-bundle. Construct [on name [...]] and continue lazy-listing onfiles.
                    (cons (create-item file-bundle)
                          (find-saved-oplixes onfiles ffname sfname))
                    ;; No file-bundle.
                    (find-saved-oplixes onfiles ffname sfname))))
              ;; No more onfiles.
              (when (seq file-bundle)
                (list (create-item file-bundle))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-stdio
  []
  [*in* *out* *err*])

(defn get-base-class-loader
  []
  *base-class-loader*)

(defn get-oplix-jvm-and-jar-paths
  [on]
  (let [pa [(get-oplix-dir on (get-default :src :oplix :jvm :dir-name))]]
    (reduce (fn [a p] (conj a p)) pa (find-files '.jar (first pa)))))

(defn create-oplix-class-loader
  [on]
  (let [cps (conj (get-oplix-jvm-and-jar-paths on) (get-dop-classes-dir))
        cpn (count cps)
        urls (make-array URL cpn)]
    (reduce (fn [a i] (aset a i (.toURL (.toURI (cps i)))) a) urls (range cpn))
    (clojure.lang.DynamicClassLoader. (URLClassLoader. urls (get-base-class-loader)))))

(defn load-oplix-class
  [oplix fqcn]
  (.loadClass (oplix-cl oplix) (str fqcn)))

(defn make-oplix-class-instance
  [oplix fqcn]
  (.newInstance (load-oplix-class oplix fqcn)))

(defmacro with-oplix-context
  [on oplix-class-loader return-value-when-exception & body]
  `(let [ct# (Thread/currentThread)
         ccl# (.getContextClassLoader ct#)]
     (try
       (.setContextClassLoader ct# ~oplix-class-loader)
       ~@body
       (catch Exception e#
         (log-exception e# (get-oplix-fqns ~on))
         ~return-value-when-exception)
       (finally
        (.setContextClassLoader ct# ccl#)))))

(defn reload-on?
  ([on]
     (let [fqon (get-oplix-fqns on)]
       (require fqon :reload)
       (if (find-ns fqon)
         true
         false)))
  ([on cl]
     (with-oplix-context on cl false
       (reload-on? on))))

(defn- -aot-compiler
  [on aot verbose? cl-dump?]
  (let [aotf (File. (get-oplix-dir on) (str (nssym2path aot) ".clj"))]
    (if (.exists aotf)
      (let [cl (create-oplix-class-loader on)]
        (with-oplix-context on cl false
          (when cl-dump?
            (loop [cl cl]
              (when cl
                (println cl)
                (print-seq (seq (.getURLs cl)))
                (recur (.getParent cl)))))
          ;;
          (let [cp (get-dop-classes-dir)
                fqaot (get-oplix-fqns on aot)]
            (binding [*compile-path* (str cp)]
              (compile fqaot)
              true))))
      (let [s (str "aot-compile?: not found aot file: " aotf)]
        (when verbose?
          (log-warning s)
          (print-warning s))
        false))))

(defn aot-compile?
  ([on]
     (aot-compile? on 'aot true))
  ([on aot]
     (aot-compile? on aot true))
  ([on aot verbose?]
     (aot-compile? on aot verbose? false))
  ([on aot verbose? cl-dump?]
     (let [-aot-compiler-bfn (bound-fn [] (-aot-compiler on aot verbose? cl-dump?))
           ac (future (-aot-compiler-bfn))]
       @ac)))

(defn invoke-later
  "Take a body of expressions, post it to the event dispatch thread of the
   current or specified oplix, and return with nil immedidately when wait?
   is false. The body will be evaluated later in the EDT."
  ([body]
     (invoke-later *oplix* body))
  ([oplix body]
     (invoke-later oplix body false))
  ([oplix body wait?]
     (binding [*oplix* oplix]
       (if-let [app-context (:app-context (oplix-context oplix))]
         (invoke-later-in-oplix-context oplix body wait?)
         (alt-invoke-later-in-oplix-context oplix body wait?)))
     nil))

(defn invoke-and-wait
  "Call invoke-later with true for wait?. Return nil."
  ([body]
     (invoke-and-wait *oplix* body))
  ([oplix body]
     (invoke-later oplix body true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load/save oplix frame
;;;; - DONOT CALL THESE FUNCTIONS DIRECTLY. CALL VIA INVOKE-LATER OR
;;;;   INVOKE-AND-WAIT.
;;;; - These save and load oplix frame only. Saving and loading oplix instance
;;;;   data should be done by the oplix event handlers in response to save/load
;;;;   events.
;;;; - No need to setup per-oplix CL because these are (have to be) called in
;;;;   oplix's EDT where per-oplix CL is set up.
;;;;
;;;; Note: make sure to use the same CL used for reload-on?, or XMLEncoder
;;;; would go into infinite recursive calls.

(defn- -load-oplix-frame
  ([oplix]
     (-load-oplix-frame (oplix-on oplix) (oplix-name oplix)))
  ([on name]
     (let [[f s] (get-oplix-file-bundle on name)]
       (when (and (.exists f) (.canRead f))
         (with-open [s (BufferedInputStream. (FileInputStream. f))
                     d (XMLDecoder. s)]
           (try
             (.readObject d)
             (catch Exception e
               (log-exception e)
               nil)))))))

(defn- -presave-oplix-frame
  [oplix]
  (let [frame (oplix-frame oplix)
        presave-oplix-frame-os-value (presave-oplix-frame-os frame)]
    [#(postsave-oplix-frame-os frame presave-oplix-frame-os-value)]))

(defn- -postsave-oplix-frame
  [postsave-oplix-frame-fns]
  (doseq [pofn postsave-oplix-frame-fns]
    (pofn)))

(defn- -save-oplix-frame?
  [oplix log-xml-encoder-errors?]
  (let [postsave-oplix-frame-fns (-presave-oplix-frame oplix)]
    (try
      (with-create-on-get-dir
        (let [[f s] (get-oplix-file-bundle (oplix-on oplix) (oplix-name oplix))]
          (when (.exists f)
            (.delete f))
          (with-open [xe (XMLEncoder. (BufferedOutputStream. (FileOutputStream. f)))]
            (when-not log-xml-encoder-errors?
              (let [el (proxy [ExceptionListener] [] (exceptionThrown [e]))]
                (.setExceptionListener xe el)))
            (.writeObject xe (oplix-frame oplix))
            (.flush xe))
          true))
      (catch Exception e
        (log-exception e)
        false)
      (finally
       (-postsave-oplix-frame postsave-oplix-frame-fns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn register-oplix
  ([oplix]
     (register-oplix oplix (oplix-name oplix)))
  ([oplix name]
     (dosync
      (ref-set *oplixes* (assoc @*oplixes* (str name) oplix)))))

(defn unregister-oplix
  ([oplix]
     (unregister-oplix oplix (oplix-name oplix)))
  ([oplix name]
     (remove-from-xref oplix)
     (dosync
      (ref-set *oplixes* (dissoc @*oplixes* (str name))))))

(defn- -oplix-is-opening
  [name opening?]
  (dosync
   (ref-set *opening-oplix-names* (if opening?
                                    (conj @*opening-oplix-names* name)
                                    (disj @*opening-oplix-names* name)))))

(defn- -is-oplix-opening?
  ([name]
     (contains? @*opening-oplix-names* name))
  ([name opening?]
     (if (contains? @*opening-oplix-names* name)
       true
       (do
         (when opening?
           (-oplix-is-opening name true))
         false))))

(defn is-singleton-oplix?
  [object]
  (let [on (if (is-oplix? object)
             (oplix-on object)
             (symbol (str object)))
        fon (filter #(= on %) @*oplix-on-cache*)]
    (if (seq fon)
      (true? (:singleton (get-oplix-on-meta (first fon))))
      false)))

(defn- -create-initial-frame
  [oplix]
  (let [f (JFrame.)
        n (oplix-name oplix)]
    (doto f
      (.setLocationByPlatform *set-location-by-platform*)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setTitle (str n))
      (.setSize (get-default :frame :width) (get-default :frame :height)))
    f))

(defn- -abort-open-oplix
  ([oplix]
     (-abort-open-oplix oplix
                        :openar.event/oplix-error-open
                        :openar.event/reason-exception-occurred))
  ([oplix eid reason]
     (-abort-open-oplix oplix eid reason true))
  ([oplix eid reason post-event?]
     (-oplix-is-opening (oplix-name oplix) false)
     (when (identical? (get-oplix (oplix-name oplix)) oplix)
       (unregister-oplix oplix))
     (when-let [frame (oplix-frame oplix)]
       (.dispose frame))
     (when-let [app-context (:app-context (oplix-context oplix))]
       (dispose-app-context app-context))
     (when post-event?
       (post-event eid oplix (if (and (map? reason) (:reason reason))
                               reason
                               {:reason reason})))
     (when (= reason :openar.event/reason-singleton-oplix)
       (.toFront (oplix-frame (first (get-oplixes (oplix-on oplix))))))
     eid))

(defmacro -send-event-and-continue-unless
  [deny-res oplix eid send-fn & body]
  `(let [resps# (~send-fn ~eid ~oplix)
         [res# rsn#] (get-event-response (get resps# (oplix-name ~oplix)))]
     (if (= res# :openar.event/response-exception-occurred)
       (-abort-open-oplix ~oplix)
       (if (and ~deny-res (= ~deny-res res#))
         (-abort-open-oplix ~oplix :openar.event/oplix-open-canceled rsn#)
         (do
           ~@body)))))

(defn- -open-oplix
  "Open oplix synchronously."
  ([oplix io]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [on (oplix-on oplix)
             name (oplix-name oplix)
             oeo-eid :openar.event/oplix-error-open]
         (if (or (-is-oplix-opening? name true) (get-oplix name))
           ;; name exists
           (-abort-open-oplix oplix oeo-eid :openar.event/reason-name-exists)
           ;; continue opening
           (if (not (and (contains? (get-all-oplix-on) on) (reload-on? on (oplix-cl oplix))))
             ;; reload-on failed
             (-abort-open-oplix oplix oeo-eid :openar.event/reason-reload-on-failed)
             ;; continue opening
             (if (and (is-singleton-oplix? oplix) (get-oplixes on))
               ;; singleton exists
               (-abort-open-oplix oplix oeo-eid :openar.event/reason-singleton-oplix)
               ;; continue opening
               (let [saved? (if (is-oplix-saved oplix) true false)]
                 ;; opening
                 (-send-event-and-continue-unless
                  :openar.event/response-donot-open
                  oplix :openar.event/oplix-opening send-creation-event
                  (if saved?
                    ;; load frame
                    (let [frame (atom nil)
                          frame-loader #(reset! frame (-load-oplix-frame oplix))]
                      (-send-event-and-continue-unless
                       nil ;; cannot deny loading frame for now
                       oplix :openar.event/oplix-frame-loading send-creation-event
                       (invoke-and-wait oplix frame-loader)
                       (if @frame
                         (-open-oplix oplix saved? @frame)
                         ;; load frame failed
                         (let [rsn :openar.event/reason-load-frame-failed]
                           (post-creation-event oeo-eid oplix rsn)
                           (-abort-open-oplix oplix oeo-eid rsn false)))))
                    ;; create frame (never fail)
                    (let [frame (atom nil)
                          frame-creator #(reset! frame (-create-initial-frame oplix))]
                      (-send-event-and-continue-unless
                       nil ;; cannot deny creating frame for now
                       oplix :openar.event/oplix-frame-creating send-creation-event
                       (invoke-and-wait oplix frame-creator)
                       (-open-oplix oplix saved? @frame))))))))))))
  ([oplix saved? frame]
     ;; Install the default listeners.
     (when-not saved?
       (doto frame
         (add-default-window-listener)
         (add-default-key-listener)))
     (let [oplix (assoc oplix :frame frame)
           eid (if saved?
                 :openar.event/oplix-frame-loaded
                 :openar.event/oplix-frame-created)]
       ;; frame created or loaded
       (-send-event-and-continue-unless
        nil ;; ignore any response
        oplix eid send-creation-event
        (register-oplix oplix)
        (-send-event-and-continue-unless
         nil ;; ditto
         oplix :openar.event/oplix-opened post-event
         ;; oplix opened, finally.
         ;; If the frame is newly created, change the default close operation
         ;; to do nothing and let Openar handle the close operation.
         (when-not saved?
           (.setDefaultCloseOperation frame JFrame/DO_NOTHING_ON_CLOSE))
         (-oplix-is-opening (oplix-name oplix) false)
         :openar.event/oplix-opened)))))

(defn- -get-context-and-start-oplix-creation
  ([oplix]
     (if-let [app-context (create-app-context (oplix-name oplix) (oplix-cl oplix))]
       ;; EDT per oplix
       (-get-context-and-start-oplix-creation oplix (create-oplix-context app-context))
       ;; sharing the same, main EDT
       (-get-context-and-start-oplix-creation oplix (create-oplix-context))))
  ([oplix context]
     (let [oplix (assoc oplix :context context)]
       (future (-open-oplix oplix (get-stdio))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def -open-oplix-args- nil)

(defn generate-oplix-name
  [on]
  (let [Name (apply str (.toUpperCase (str (first (str on)))) (rest (str on)))]
    (if-not (or (-is-oplix-opening? Name) (get-oplix Name))
      Name
      (loop [X 1]
        (let [NameX (str Name X)]
          (if-not (get-oplix NameX)
            NameX
            (recur (inc X))))))))

(defn open-oplix
  "Return a future oject that creates an oplix instance using oplix name on
   and notifies open events to it. Instance name is optional."
  ([on]
     (open-oplix on (generate-oplix-name on)))
  ([on name]
     (let [on (symbol on)
           name (str name)
           cl (create-oplix-class-loader on)
           oplix {:oid (gensym 'oid) :on on :name name :cl cl :args -open-oplix-args-}]
       (-get-context-and-start-oplix-creation oplix))))

(defn open-oplix-and-wait
  "Return the dereference to the future object returned from the open-oplix
   call with oplix name on. Instance name is optional."
  ([on]
     (open-oplix-and-wait on (generate-oplix-name on)))
  ([on name]
     (let [opener (open-oplix on name)]
       @opener)))

(defn open-all-oplixes-and-wait
  ([]
     (open-all-oplixes-and-wait false))
  ([startup?]
     (let [sf (get-oplix-startup-file)
           ons (if (and startup? (.exists sf))
                 (try
                   (read-string (slurp sf :encoding "UTF-8"))
                   (catch Exception e
                     (log-warning e)
                     nil))
                 (map (fn [[o n [f s]]] [o n]) (find-saved-oplixes)))]
       (doseq [[on name] ons]
         ;; Exclude the oplix 'Openar' because it's special and is opened
         ;; at the startup time.
         (declare is-oplix-openar?)
         (when-not (is-oplix-openar? on name)
           (when (is-oplix-saved on name)
             (open-oplix-and-wait on name)))))))

(defmacro open-oplix-with-args
  "Return a future oject that opens an oplix instance using oplix name on
   and arguments contained in an object args and notifies open events to it.
   Instance name is optional."
  ([args on]
     `(binding [-open-oplix-args- ~args]
        (open-oplix ~on)))
  ([args on name]
     `(binding [-open-oplix-args- ~args]
        (open-oplix ~on ~name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -save-oplix
  "Save oplix (meaning, frame) synchronously."
  ([oplix io]
     (-save-oplix oplix io false))
  ([oplix io save-on-close?]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [info {:openar.event/info-save-on-close save-on-close?}]
         (clear-saved-dynaclass-listeners oplix)
         (let [resps (send-event :openar.event/oplix-saving oplix info)
               [res rsn] (get-event-response (get resps (oplix-name oplix)))]
           (if (= res :openar.event/response-exception-occurred)
             ;; save failed 
             (let [eid :openar.event/oplix-error-save]
               (restore-saved-dynaclass-listeners oplix)
               (post-event eid oplix (merge info {:reason :openar.event/reason-exception-occurred}))
               eid)
             (if (= res :openar.event/response-donot-save)
               ;; save canceled
               (let [eid :openar.event/oplix-save-canceled]
                 (post-event eid oplix (merge info {:reason rsn}))
                 eid)
               ;; continue saving
               (let [saved? (atom false)
                     log? (if (= res ::openar.event/response-suppress-xml-encoder-errors)
                            false
                            true)]
                 (invoke-and-wait oplix #(reset! saved? (-save-oplix-frame? oplix log?)))
                 (let [eid (if @saved?
                             :openar.event/oplix-saved
                             :openar.event/oplix-error-save)]
                   (restore-saved-dynaclass-listeners oplix)
                   (post-event eid oplix info)
                   eid)))))))))

(defn save-oplix
  "Return a future object that notifies save events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string. Return nil when object is invalid."
  [object]
  (when-let [oplix (get-oplix object)]
    (future (-save-oplix oplix (get-stdio)))))

(defn save-oplix-and-wait
  "Create a future object that notifies save events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [object]
  (when-let [saver (save-oplix object)]
    @saver))

(defn save-all-oplixes-and-wait
  "Wait for all oplixes saved."
  []
  (doseq [name (get-oplix-names)]
    (save-oplix-and-wait name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -close-oplix
  "Close oplix synchronously."
  ([oplix io]
     (-close-oplix oplix io false))
  ([oplix io close-on-delete?]
     (binding [*in* (first io) *out* (second io) *err* (last io)]
       (let [info {:openar.event/info-close-on-delete close-on-delete?}
             lge (get-last-global-event)
             [can-close? reason] (if (or close-on-delete? ;; cannot deny closing with these conditions
                                         (= lge :openar.event/oplixes-closing)
                                         (= lge :openar.event/openar-quitting))
                                   (let [info (cond
                                               (= lge :openar.event/oplixes-closing) {:openar.event/info-close-on-close-oplixes true}
                                               (= lge :openar.event/openar-quitting) {:openar.event/info-close-on-quit-openar true}
                                               :else info)]
                                     (send-event :openar.event/oplix-closing oplix info)
                                     [true nil])
                                   (let [resps (send-event :openar.event/oplix-closing oplix info)
                                         [res rsn] (get-event-response (get resps (oplix-name oplix)))]
                                     (if (= res :openar.event/response-exception-occurred)
                                       [false :openar.event/reason-exception-occurred]
                                       (if (= res :openar.event/response-donot-close)
                                         [false rsn]
                                         [true nil]))))]
         (if-not can-close?
           ;; close canceled or close error by exception
           (let [eid (if (= reason :openar.event/reason-exception-occurred)
                       :openar.event/oplix-error-close
                       :openar.event/oplix-close-canceled)]
             (post-event eid oplix (merge info {:reason reason}))
             eid)
           ;; continue closing
           (let [so (-save-oplix oplix io true)]
             (if (and (not close-on-delete?) ;; ignore close error when deleting
                      (= so :openar.event/oplix-error-save))
               ;; close error
               (let [eid :openar.event/oplix-error-close
                     rsn :openar.event/reason-save-error-on-closing]
                 (post-event eid oplix (merge info {:reason rsn}))
                 eid)
               ;; closed
               (let [eid :openar.event/oplix-closed]
                 ;; Unregister the oplix. Then dispose its frame and
                 ;; optionally its app-context.
                 (unregister-oplix oplix)
                 (.dispose (oplix-frame oplix))
                 (when-let [ac (:app-context (oplix-context oplix))]
                   (dispose-app-context ac))
                 ;;
                 (post-event-to oplix eid oplix info)
                 (post-event eid oplix info)
                 eid))))))))

(defn close-oplix
  "Return a future object that notifies close events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string. Return nil when object is invalid."
  [object]
  (when-let [oplix (get-oplix object)]
    (future (-close-oplix oplix (get-stdio)))))

(defn close-oplix-and-wait
  "Create a future object that notifies close events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  [object]
  (when-let [closer (close-oplix object)]
    @closer))

(defn close-all-oplixes-and-wait
  "Wait for all oplixes closed."
  ([]
     (close-all-oplixes-and-wait false))
  ([shutdown?]
     ;; Exclude the oplix 'Openar' because it's special and is closed at
     ;; the shutdown time.
     (let [exclude-fn (fn [col] (filter #(not (is-oplix-openar? %)) col))
           all-oplixes (exclude-fn (get-oplixes))
           vis-oplixes (exclude-fn (map #(get-oplix %) (get-z-ordered-frames)))
           unv-oplixes (clojure.set/difference (apply hash-set all-oplixes)
                                              (apply hash-set vis-oplixes))]
       (doseq [oplix all-oplixes]
         (close-oplix-and-wait oplix))
       (when shutdown?
         (let [ons (map #(vector (oplix-on %) (oplix-name %))
                        (concat unv-oplixes (reverse vis-oplixes)))
               osf (get-oplix-startup-file)]
           (when (.exists osf)
             (.delete osf))
           (spit osf (print-str ons) :encoding "UTF-8"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -delete-oplix
  [on name io]
  (binding [*in* (first io) *out* (second io) *err* (last io)]
    (let [info {:on on :name name}
          eid :openar.event/oplix-deleting]
      ;; deleting
      (when-let [oplix (get-oplix name)]
        ;; Close the running oplix forcibly.
        (send-event eid oplix)
        (-close-oplix oplix io true))
      (send-event eid nil info)
      (if (trash-dir? (get-dop-oplix-name-dir on name) (get-dop-oplix-dir))
        ;; deleted
        (let [eid :openar.event/oplix-deleted]
          (post-event eid nil info)
          eid)
        ;; delete failed
        (let [eid :openar.event/oplixe-error-delete
              rsn :openar.event/reason-trash-files-failed]
          (post-event eid nil (merge info {:reason rsn}))
          eid)))))

(defn delete-oplix
  "Return a future object that notifies delete events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string. Return nil when object is invalid."
  ([object]
     (when-let [oplix (get-oplix object)]
       (delete-oplix (oplix-on oplix) (oplix-name oplix))))
  ([on name]
     (future (-delete-oplix (symbol on) name (get-stdio)))))

(defn delete-oplix-and-wait
  "Create a future object that notifies delete events to oplix instance
   specified by object, which can be oplix instance or instance name in
   symbol or string, and return the dereference to it. Return nil when
   object is invalid."
  ([object]
     (when-let [deleter (delete-oplix object)]
       @deleter))
  ([on name]
     (when-let [deleter (delete-oplix on name)]
       @deleter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- generate-oplix-code
  [on]
  (format (str "(ns ^{:oplix true}\n"
               "  oplix.%s\n"
               "  (:use [openar config core event log oplix ui utils]))\n\n"
               "(defn opened\n"
               "  [event]\n"
               "  (set-oplix-visible))\n") on))

(defn create-oplix-file?
  [oplix-file on]
  (try
    (spit oplix-file (generate-oplix-code on) :encoding "UTF-8")
    true
    (catch Exception e
      (log-exception e)
      false)))

(defn create-oplix
  [on]
  (let [info {:on on}]
    ;; creating
    (send-event :openar.event/oplix-creating nil info)
    (let [sod (get-oplix-dir)
          oplix-file (File. sod (str (nssym2path on) ".clj"))
          on-dir (.getParentFile oplix-file)]
      (if (.exists oplix-file)
        ;; create failed
        (let [eid :openar.event/oplix-error-create]
          (send-event eid nil (assoc info :reason :openar.event/reason-oplix-file-exists))
          eid)
        (let [eid :openar.event/oplix-created]
          (.mkdirs on-dir)
          (if (create-oplix-file? oplix-file on)
            ;; created
            (let [eid :openar.event/oplix-created]
              (add-to-oplix-on-cache on)
              (post-event eid nil info)
              eid)
            ;; create failed
            (let [eid :openar.event/oplix-error-create
                  rsn :openar.event/reason-create-oplix-file-failed]
              (send-event eid nil (assoc info :reason rsn))
              eid)))))))

(defn purge-oplix
  "Purge oplix and instance files. Return a purge event id, or nil.
   Cannot purge if instance is running."
  [on]
  (let [src-on-file (get-oplix-file on)]
    (when (.exists src-on-file)
      (let [info {:on on}]
        ;; purging
        (send-event ::openar.event/oplix-purging nil info)
        (let [oplixes (filter #(= on (oplix-on %)) (get-oplixes))]
          (if (seq oplixes)
            ;; purge failed
            (let [eid :openar.event/oplix-error-purge
                  rsn :openar.event/reason-oplix-running]
              (post-event eid nil (assoc info :reason rsn))
              eid)
            ;; continue purging
            (let [trash-sof? (trash-file? src-on-file)
                  trash-sod? (trash-dir? (get-oplix-dir on) (get-src-dir))
                  trash-dod? (trash-dir? (get-dop-oplix-dir on) (get-dop-oplix-dir))]
              (if (and trash-sof? trash-sod? trash-dod?)
                ;; purged
                (let [eid :openar.event/oplix-purged]
                  (remove-from-oplix-on-cache on)
                  (delete-dir? (get-dop-classes-oplix-dir on) (get-dop-classes-dir))
                  (post-event eid nil info)
                  eid)
                ;; Purge failed
                (let [eid :openar.event/oplix-error-purge
                      rsn :openar.event/reason-trash-files-failed]
                  (post-event eid nil (assoc info
                                        :reason rsn
                                        :status {:src-on-file trash-sof?
                                                 :src-oplix-dir trash-sod?
                                                 :dop-oplix-dir trash-dod?}))
                  eid)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-oplix-openar-on
  []
  (get-default :on :openar))

(defn open-oplix-openar-and-wait
  []
  (open-oplix-and-wait (get-oplix-openar-on) (get-openar-name)))

(defn close-oplix-openar-and-wait
  []
  (close-oplix-and-wait (get-openar-name)))

(defn get-oplix-openar
  []
  (get-oplix (get-openar-name)))

(defn update-oplix-openar-lists
  []
  (let [oplix-openar (get-oplix-openar)]
    (when-let [update-lists-fn (:update-lists-fn (xref-with oplix-openar))]
      (when (fn? (var-get update-lists-fn))
        (invoke-later oplix-openar update-lists-fn)))))

(defn is-oplix-openar?
  ([object]
     (if-let [oplix (get-oplix object)]
       (is-oplix-openar? (oplix-on oplix) (oplix-name oplix))
       false))
  ([on name]
    (if (and (= (symbol on) (get-oplix-openar-on))
             (= (str name) (get-openar-name)))
      true
      false)))

(defn can-oplix-openar-close?
  []
  *oplix-openar-can-close*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-oplix-title
  ([title]
     (set-oplix-title *oplix* title))
  ([oplix title]
     (when-let [frame (oplix-frame oplix)]
       (.setTitle frame (str title))
       (update-oplix-openar-lists))))

(defn set-oplix-visible
  ([]
     (set-oplix-visible *oplix* true))
  ([oplix]
     (set-oplix-visible oplix true))
  ([oplix visible?]
     (.setVisible (oplix-frame oplix) visible?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -acquire-base-class-loader?
  []
  (when-not *base-class-loader*
    (reset-base-class-loader (.getContextClassLoader (Thread/currentThread))))
  true)

(defn create-dop-oplix-dirs?
  []
  (get-dop-classes-dir)
  (get-dop-oplix-dir)
  (get-dop-trash-dir)
  true)

(defn create-src-library-dirs?
  []
  (get-library-dir)
  (get-library-oplix-dir)
  true)

(defn cache-oplix-ons?
  []
  (reset! *oplix-on-cache* (apply conj @*oplix-on-cache* (find-all-oplix-on)))
  true)

(defn register-exception-listeners?
  []
  (doseq [on (get-all-oplix-on)]
    (when-let [nm (:exception-listener (if (find-ns (get-oplix-fqns on))
                                         (get-oplix-on-meta on)
                                         (meta on)))]
      (if (symbol? nm)
        (register-exception-listener on nm)
        (when (and (seq nm)
                   (symbol? (last nm)))
          (register-exception-listener on (last nm))))))
  true)

(defn aot-compile-oplixes?
  []
  (doseq [on (get-default :startup :aot-compile-list)]
    (aot-compile? on 'aot false))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn shutdown-oplix?
  []
  true)

(defn startup-oplix?
  []
  (with-create-on-get-dir
    (and true
         (-acquire-base-class-loader?)
         (create-dop-oplix-dirs?)
         (create-src-library-dirs?)
         (cache-oplix-ons?)
         (register-exception-listeners?)
         (aot-compile-oplixes?))))
