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

(ns ^{:doc "Sevenri system core library"}
  sevenri.core
  (:require [clojure.java io])
  (:use [sevenri config defs log refs utils]
        [sevenri.os :only (get-ignorable-file-names-os)])
  (:import (java.io BufferedWriter File FileOutputStream)
           (java.io InputStreamReader OutputStreamWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Symbol to/from path translation
;;
;; First, there is the Clojure's translation rule for translating symbol to
;; path; period '.' to slash '/' and hyphen '-' to underscoe '_'. Then, the
;; Sevenri own rule, that is, underscore '_' to space ' ' and exclamation
;; '!' to period '.'.
;; In addition, there are certain characters that are legal for symbol but
;; not for some file systems. Those 'unsafe' chars are translated to a
;; symbol-to-path proxy char.
;; Translation from path to symbol goes in backward but not completely
;; symmetrically. The symbol-to-path proxy char in path is translated to
;; path-to-symbol proxy char, which is one of the unsafe chars, so that
;; the symbol can be retro-translated to the original path.

(def *unsafe-chars* "[*?:|<>\"\\\\]") ;; * ? : | < > " \
(def *sym2path-proxy* "!")
(def *path2sym-proxy* "?")

;;;;

(defn sym2path
  "Return a pathname string, not a File object."
  [^clojure.lang.Symbol sym]
  (-> (str sym)
      (.replace \_ \space)
      (.replace \- \_)
      (.replace \. \/)
      (.replace \! \.)
      (.replaceAll *unsafe-chars* *sym2path-proxy*)))

(defn path2sym
  "path should be either a pathname or a File object. Return a symbol."
  [path]
  (-> (str path)
      (.replace *sym2path-proxy* *path2sym-proxy*)
      (.replace \. \!)
      (.replace \/ \.)
      (.replace \_ \-)
      (.replace \space \_)
      (symbol)))

(defn obj2sym
  "Convert object to symbol, expecting obj is a non-pathname and the result
   would follow the Sevenri's symbol format (underscore and exclamation mean
   space and period respectively). Use path2sym when obj is a pathname."
  [obj]
  (if (instance? File obj)
    (path2sym obj)
    (-> (.replaceAll (str obj) "\\s" "_")
        (symbol))))

;;;;

(defn safesym?
  "Return true when the symbol is reversible from sym2path."
  [^clojure.lang.Symbol sym]
  (= sym (path2sym (sym2path sym))))

(defn safepath?
  "Return true when the path is reversible from path2sym."
  [path]
  (= path (sym2path (path2sym path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:doc "Control path creation on get-path call"} *make-path* false)

;;;;

(defn get-path
  "Return a File object based on parent and optional child paths. Each path
   is either a symbol or some object, normally a File object or string. 
   If it's a symbol, the sym2path translation is applied. Otherwise,
   string representation of the path is used.
   If *make-path* is true and the target path doesn't exist, it is created
   before returning."
  [parent-path & child-paths]
  (let [path (reduce (fn [p c]
                       (File. p (if (symbol? c)
                                  (sym2path c)
                                  (str c))))
                     (if (instance? File parent-path)
                       parent-path
                       (File. (if (symbol? parent-path)
                                (sym2path parent-path)
                                (str parent-path))))
                     child-paths)]
    (when (and *make-path* (not (.exists path)))
      (when-not (.mkdirs path)
        (throw (RuntimeException. (str "get-path: making path failed: " path)))))
    path))

;;;;

(defn empty-path?
  "Return true when path doesn't exist or is a directory containing no or
   any ignorable files."
  [path]
  (let [path (get-path path)
        files (.listFiles path)]
    (if (or (nil? files) (zero? (alength files)))
      true
      (let [ifns (get-ignorable-file-names-os)]
        (if (every? #(contains? ifns (.getName %)) (seq files))
          true
          false)))))

(defn remove-path?
  "Return true when the specified path doesn't exist or when it exists but is
   removed successfully."
  [path]
  (let [p (get-path path)]
    (if (.exists p)
      (if (.isDirectory p)
        (and (every? true? (map remove-path? (.listFiles p))) (.delete p))
        (.delete p))
      true)))

;;;;

(defmacro with-make-path
  "Let get-path create the path when it doesn't exist."
  [& body]
  `(binding [*make-path* true]
     ~@body))

(defmacro without-make-path
  "Do opposite of with-make-path; turn off of making path on get-path."
  [& body]
  `(binding [*make-path* false]
     ~@body))

;;;;

(defmacro defgp
  "Macro to define get-path variant"
  [get-x-path parent-path]
  `(defn ~get-x-path
     [& ~'child-paths]
     (apply get-path ~parent-path ~'child-paths)))

(defgp get-doc-path (get-path (get-user-path) (get-config 'doc.dir-name)))
(defgp get-lib-path (get-path (get-user-path) (get-config 'lib.dir-name)))
(defgp get-src-path (get-path (get-user-path) (get-config 'src.dir-name)))

(defgp get-src-library-path (get-path (get-src-path) (get-config 'src.library.dir-name)))
(defmacro get-library-path [& paths] `(get-src-library-path ~@paths))

(defgp get-src-project-path (get-path (get-src-path) (get-config 'src.project.dir-name)))
(defmacro get-project-path [& paths] `(get-src-project-path ~@paths))

(defgp get-src-resources-path (get-path (get-src-path) (get-config 'src.resources.dir-name)))
(defmacro get-resources-path [& paths] `(get-src-resources-path ~@paths))

(defgp get-src-sevenri-path (get-path (get-src-path) (get-config 'src.sevenri.dir-name)))
(defmacro get-sevenri-path [& paths] `(get-src-sevenri-path ~@paths))

(defgp get-temp-path (get-path (get-user-path) (get-config 'temp.dir-name)))

(defgp get-sid-path *sid-path*)
(defgp get-sid-classes-path (get-sid-path (get-config 'sid.classes.dir-name)))
(defgp get-sid-sevenri-path (get-sid-path (get-config 'sid.sevenri.dir-name)))
(defgp get-sid-temp-path (get-sid-path (get-config 'sid.temp.dir-name)))
(defgp get-sid-trash-path (get-sid-path (get-config 'sid.trash.dir-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Temporary file creator fns

(defn get-temp-file*
  ([temp-path]
     (get-temp-file* temp-path 'tmp))
  ([temp-path prefix]
     (get-temp-file* temp-path prefix '!tmp))
  ([temp-path prefix ext]
     ;; File/createTempFile requires prefix is at least 3 char long.
     (let [strpfx (str prefix)
           prefix (if (< 2 (count strpfx))
                    strpfx
                    (subs (.concat strpfx "___") 0 3))]
       (File/createTempFile prefix (sym2path ext) temp-path))))

(defn get-temp-file
  "Create and return a temp file in Sevenri.Temp directory."
  ([]
     (get-temp-file* (get-temp-path)))
  ([prefix]
     (get-temp-file* (get-temp-path) prefix))
  ([prefix ext]
     (get-temp-file* (get-temp-path) prefix ext)))
     
(defn get-sid-temp-file
  "Create and return a temp file in !sevenri.temp directory."
  ([]
     (get-temp-file* (get-sid-temp-path)))
  ([prefix]
     (get-temp-file* (get-sid-temp-path) prefix))
  ([prefix ext]
     (get-temp-file* (get-sid-temp-path) prefix ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Trashing path fns

(defn trash-path?
  "Move path into .sevenri/trash, preserving its relative path based on its
   location."
  ([path]
     (let [path (get-path path)]
       (if (re-find (re-pattern (str "^" (get-sid-path))) (.getPath path))
         ;; A sid path is moved to a relative location in !sevenri.trash.sid.
         (trash-path? path
                      (get-sid-trash-path (get-config 'sid.trash.sid.dir-name)
                                          (File. (subs (.getPath path)
                                                       (inc (count (str (get-sid-path))))))))
         ;; A path under the user directory is moved to a relative location in !sevenri.trash.
         (if (re-find (re-pattern (str "^" (get-user-path))) (.getPath path))
           (trash-path? path
                        (get-sid-trash-path (File. (subs (.getPath path)
                                                         (inc (count (str (get-user-path))))))))
           ;; Any other path is moved into !sevenri.trash with its original path preserved.
           (trash-path? path
                        (get-sid-trash-path path))))))
  ([src-path dst-path]
     (let [spath (get-path src-path)
           dpath (get-path dst-path)
           dst-clean? (remove-path? dpath)
           dst-parent (.getParentFile dpath)]
         (.mkdirs dst-parent)
         (if (and dst-clean? (.exists dst-parent))
           (or (.renameTo spath dpath)
               ;; Some platforms, like Mac, don't allow to renameTo shared files
               ;; but copy only.
               (try
                 (clojure.java.io/copy spath dpath)
                 (when (.exists dpath)
                   (.delete spath))
                 (if (or (.exists spath) (not (.exists dpath)))
                   (do
                     (log-warning "trash-path? failed. spath:" spath "dpath:" dpath)
                     false)
                   true)
                 (catch Exception e
                   (log-warning "trash-path? exception. spath:" spath "dpath:" dpath)
                   false)))
           (do
             (when-not dst-clean? (log-warning "trash-path? failed to clean:" dpath))
             (when-not (.exists dst-parent) (log-warning "trash-path? failed to make:" dst-parent))
             false)))))

(defn clean-path?
  "Trash the specified path. Then remove the parent path if it became empty as
   the result of trashing. Continue the process up to upto-path."
  ([path upto-path]
     (let [path (get-path path)
           upto-path (get-path upto-path)]
       (if (neg? (.compareTo path upto-path))
         (do
           (log-warning "clean-path? failed: path < upto-path")
           false)
         (if (trash-path? path)
           (clean-path? path (.getParentFile path) upto-path)
           false))))
  ([_ curr-path upto-path]
     ;; Return true no matter what because trashing the path in question is
     ;; completed and the rest of the task is not important.
     (loop [curr-path (get-path curr-path)
            upto-path (get-path upto-path)]
       (if (pos? (.compareTo curr-path upto-path))
         (do
           (when (empty-path? curr-path)
             (trash-path? curr-path))
           (recur (.getParentFile curr-path) upto-path))
         true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *sevenri-name* 'Sevenri)
(def *sevenri-version* "0.0.0")

;;;;

(defn get-sevenri-name
  []
  (str *sevenri-name*))

(defn read-sevenri-version
  []
  (let [f (get-src-sevenri-path 'version!clj)]
    (read-string (slurp f))))

(defn get-sevenri-version
  []
  (let [ov *sevenri-version*]
    (str (ov :major) \. (ov :minor) \. (ov :incremental))))

(defn get-sevenri-name-and-version
  []
  (str (get-sevenri-name) \- (get-sevenri-version)))

;;;;

(defn get-sevenri-namespaces
  []
  (get-config 'top-level-ns))

(defmulti is-sevenri-var?
  class)

(defmethod is-sevenri-var? clojure.lang.Var
  [vr]
  (let [nmsp (str (ns-name (:ns (meta vr))))
        opns (map #(re-pattern (str "^" % "\\..*")) (get-sevenri-namespaces))]
    (if (some #(re-matches % nmsp) opns)
      true
      false)))

(defmethod is-sevenri-var? clojure.lang.Symbol
  [sym]
  (is-sevenri-var? (resolve sym)))

(defmethod is-sevenri-var? :default
  [_]
  false)
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-project-protocol-file
  []
  (get-src-project-path (get-config 'src.project.protocol-file-name)))

(defn get-project-protocol
  ([]
     (get-project-protocol nil))
  ([kwd]
     (let [ppf (get-project-protocol-file)]
       (if (.exists ppf)
         (try
           (let [ppm (load-string (slurp ppf :encoding "UTF-8"))]
             (if (keyword? kwd)
               (get ppm kwd)
               ppm))
           (catch Exception e
             (log-severe "get-project-protocol: failed to read protocol file")
             nil))
         (do
           (log-severe "get-project-protocol: protocol file missing")
           nil)))))

(defn get-project-manager
  []
  (get-project-protocol :manager))

(defn get-project-manager-slix
  []
  (get-project-protocol :slix))

(defn query-project
  ([query-kwd slix-name]
     (query-project query-kwd slix-name nil))
  ([query-kwd slix-name name]
     (query-project query-kwd slix-name name nil))
  ([query-kwd slix-name name args]
     (when-let [prtcl (get-project-protocol)]
       (query-project query-kwd slix-name name args prtcl)))
  ([query-kwd slix-name name args protocol]
     (when (and (keyword? query-kwd)
                (or (symbol? slix-name) (string? slix-name))
                (and (map? protocol) (:manager protocol) (get protocol query-kwd)))
       (let [manager (symbol (:manager protocol))
             qryfsym (symbol (get protocol query-kwd))
             loaded? (try
                       (require manager)
                       true
                       (catch Exception e
                         (log-severe "query-project: failed to load manager:" manager
                                     "\n" (get-stack-trace-print-lines e))
                         false))
             qryfvar (when (and loaded? (find-ns manager))
                       (ns-resolve manager qryfsym))]
         (if (var? qryfvar)
           (let [m (array-map :slix-name slix-name :name name :arguments args)]
             (if (fn? (var-get qryfvar))
               (try
                 (qryfvar m)
                 (catch Exception e
                   (log-severe "query-project: fn failed:" qryfsym
                               "\n" (get-stack-trace-print-lines e))
                   nil))
               (if-let [qrymthd (get-method (var-get qryfvar) (class m))]
                 (try
                   (qrymthd m)
                   (catch Exception e
                     (log-severe "query-project: method failed:" qryfsym
                                 "\n" (get-stack-trace-print-lines e))
                     nil))
                 (do
                   (log-severe "query-project: no fn/method:" query-kwd)
                   nil))))
           (do
             (log-severe "query-project: no fn/method:" query-kwd)
             nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-code-reader
  [vr]
  (when (var? vr)
    (let [file (:file (meta vr))
          cldr (.getClassLoader (class (var-get vr)))
          strm (.getResourceAsStream cldr file)]
      (InputStreamReader. strm "UTF-8"))))

(defn get-code-of
  "Return a future which returns {:file file :line line} or nil"
  [vr]
  (when (var? vr)
    (let [{file :file line :line} (meta vr)]
      (when (and (not (empty? file)) (not= file "NO_SOURCE_PATH")
                 line (pos? line))
        (future
          (if (.exists (File. file))
            {:file file :line line}
            (try
              (when-let [rdr (get-code-reader vr)]
                (let [dst (File. (get-library-path) (str file))]
                  (.mkdirs (.getParentFile dst))
                  (with-open [wtr (BufferedWriter. (OutputStreamWriter.
                                                    (FileOutputStream. dst)
                                                    "UTF-8"))]
                    (loop [c (.read rdr)]
                      (when-not (neg? c)
                        (.write wtr c)
                        (recur (.read rdr)))))
                  (.close rdr)
                  {:file dst :line line}))
              (catch Exception e
                (log-exception e)
                nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - removed by 0.3.0

(defmulti get-prop_ (fn [& args] (class (first args))))

(defmethod get-prop_ clojure.lang.Ref
  [& rm-ks]
  (get-in (deref (first rm-ks)) (rest rm-ks)))

(defmethod get-prop_ :default
  [& ks]
  (get-in @*sevenri* ks))

(defmulti set-prop_ (fn [& args] (class (first args))))

(defmethod set-prop_ clojure.lang.Ref
  [& ref-kvs]
  (let [rf (first ref-kvs)]
    (dosync
     (ref-set rf (apply assoc (deref rf) (rest ref-kvs))))))

(defmethod set-prop_ :default
  [& kvs]
  (dosync
   (ref-set *sevenri* (apply assoc @*sevenri* kvs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn lock-and-wait
  ([obj]
     (lock-and-wait obj 0))
  ([obj timeout]
     (locking obj
       (.wait obj timeout))))

(defn unlock-and-resume
  [obj]
  (locking obj
    (.notify obj)))

(defmacro lock-run-and-wait
  [obj ^Long timeout & body]
  `(locking ~obj
     ~@body
     (.wait ~obj ~timeout)))

;;;;

(defn get-sid-sevenri-lock-file
  []
  (get-sid-sevenri-path (get-config 'sid.sevenri.lock-file-name)))

(defn create-sid-sevenri-lock-file?
  "This is called from the main/run."
  []
  (let [lock (get-sid-sevenri-lock-file)]
    (if (.exists lock)
      false
      (do
        (doto lock
          (spit "Don't disturb me")
          (.deleteOnExit))
        true))))

;;;;

(defn- -create-dirs?
  []
  (with-make-path
    (get-sid-classes-path)
    (get-sid-temp-path)
    (get-src-library-path 'user)
    (get-temp-path))
  true)

(defn- -aot-compile-sevenri-listeners?
  []
  (try
    (binding [*compile-path* (str (get-src-path))]
      (compile (get-config 'src.sevenri.listeners.aot)))
    true
    (catch Exception e
      (log-severe "-aot-compile-sevenri-listeners? failed:\n" (get-stack-trace-print-lines e))
      false)))

(defn- -setup-project-manager?
  []
  (when-let [pm (get-project-manager)]
    (when-not (query-project :ready? pm)
      (future
        (when-not (query-project :setup? pm)
          (log-severe "-setup-project-manager? failed:" pm)))))
  true)

(defn- -shutdown-project-manager?
  []
  (when-let [pm (get-project-manager)]
    (query-project :shutdown pm))
  true)

(defn- -read-sevenri-version?
  []
  (def *sevenri-version* (read-sevenri-version))
  true)

;;;;

(defn startup-core?
  []
  (-ensure-processes
   -read-sevenri-version?
   -create-dirs?
   -aot-compile-sevenri-listeners?
   -setup-project-manager?))

(defn shutdown-core?
  []
  (-ensure-processes
   -shutdown-project-manager?))
