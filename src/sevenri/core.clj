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

(ns sevenri.core
  (:use [sevenri config defs jvm log os refs startup utils])
  (:import (java.io BufferedWriter File FileOutputStream)
           (java.io InputStreamReader OutputStreamWriter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *sevenri-name* 'Sevenri)

(defn- -read-sevenri-version
  []
  (let [d (File. (get-src-dir) (str (get-default :src :sevenri :dir-name)))
        f (File. d "version.clj")]
    (read-string (slurp f))))

(def *sevenri-version* (-read-sevenri-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sevenri-name
  []
  (str *sevenri-name*))

(defn get-sevenri-version
  []
  (let [ov *sevenri-version*]
    (str (ov :major) \. (ov :minor) \. (ov :incremental))))

(defn get-sevenri-name-and-version
  []
  (str (get-sevenri-name) \- (get-sevenri-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sevenri-namespaces
  []
  (map #(symbol (name %)) (keys (get-default :tln))))

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

(defmulti get-prop (fn [& args] (class (first args))))

(defmethod get-prop clojure.lang.Ref
  [& rm-ks]
  (get-in (deref (first rm-ks)) (rest rm-ks)))

(defmethod get-prop :default
  [& ks]
  (get-in @*sevenri* ks))

(defmulti set-prop (fn [& args] (class (first args))))

(defmethod set-prop clojure.lang.Ref
  [& ref-kvs]
  (let [rf (first ref-kvs)]
    (dosync
     (ref-set rf (apply assoc (deref rf) (rest ref-kvs))))))

(defmethod set-prop :default
  [& kvs]
  (dosync
   (ref-set *sevenri* (apply assoc @*sevenri* kvs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-src-sevenri-dir
  [& pfxs]
  (reduce (fn [f p] (File. f (nssym2path p)))
          (File. (get-src-dir) (str (get-default :src :sevenri :dir-name)))
          pfxs))

(defmacro get-sevenri-dir
  [& pfxs]
  `(get-src-sevenri-dir ~@pfxs))

(defn get-src-sevenri-file
  [& pfxs]
  (File. (str (apply get-src-sevenri-dir pfxs) ".clj")))

(defmacro get-sevenri-file
  [& pfxs]
  `(get-src-sevenri-file ~@pfxs))

(defn get-sid-sevenri-dir
  [& pfxs]
  (apply get-dir (get-sid-dir (get-default :sid :sevenri :dir-name)) pfxs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-src-project-file
 [& pfxs]
 (File. (str (apply get-src-project-dir pfxs) ".clj")))

(defmacro get-project-file
  [& pfxs]
  `(get-src-project-file ~@pfxs))

(defn get-project-protocol-file
  []
  (File. (get-src-project-dir) (str (get-default :src :project :protocol-file-name))))

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
                         (log-severe "query-project: failed to load manager:" manager)
                         false))
             qryfvar (when (and loaded? (find-ns manager))
                       (ns-resolve manager qryfsym))]
         (if (var? qryfvar)
           (let [m (array-map :slix-name slix-name :name name :arguments args)]
             (if (fn? (var-get qryfvar))
               (try
                 (qryfvar m)
                 (catch Exception e
                   (log-severe "query-project: fn failed:" qryfsym)
                   nil))
               (if-let [qrymthd (get-method (var-get qryfvar) (class m))]
                 (try
                   (qrymthd m)
                   (catch Exception e
                     (log-severe "query-project: method failed:" qryfsym)
                     nil))
                 (do
                   (log-severe "query-project: no fn/method:" query-kwd)
                   nil))))
           (do
             (log-severe "query-project: no fn/method:" query-kwd)
             nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sid-temp-file
  ([]
     (get-sid-temp-file 'temp))
  ([prefix]
     (get-sid-temp-file prefix 'tmp))
  ([prefix ext]
     (let [pfxlen (count (str prefix))
           prefix (if (< 2 pfxlen)
                    (str prefix)
                    (apply str (concat (seq (str prefix)) (repeat (- 3 pfxlen) \_))))]
       (File/createTempFile prefix (str \. ext) (get-sid-temp-dir)))))

(defn get-temp-file
  ([]
     (get-temp-file 'temp))
  ([prefix]
     (get-temp-file prefix 'tmp))
  ([prefix ext]
     (let [pfxlen (count (str prefix))
           prefix (if (< 2 pfxlen)
                    (str prefix)
                    (apply str (concat (seq (str prefix)) (repeat (- 3 pfxlen) \_))))]
       (File/createTempFile prefix (str \. ext) (get-temp-dir)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trash-file?
  ([file]
     (if (re-find (re-pattern (str "^" (get-sid-root-dir))) (str file))
       (trash-file? file
                    (File. (get-sid-trash-dir (get-default :sid :trash :sid :dir-name))
                           (subs (str file) (inc (count (str (get-sid-root-dir)))))))
       (if (re-find (re-pattern (str "^" (get-user-dir))) (str file))
         (trash-file? file
                      (File. (get-sid-trash-dir)
                             (subs (str file) (inc (count (str (get-user-dir)))))))
         (trash-file? file
                      (File. (get-sid-trash-dir) (str file))))))
  ([src-file dst-file]
     (if (.isDirectory src-file)
       false
       (let [dst-clean? (if (.exists dst-file)
                          (.delete dst-file)
                          true)
             dst-parent (.getParentFile dst-file)]
         (.mkdirs dst-parent)
         (if (and dst-clean? (.exists dst-parent))
           (.renameTo src-file dst-file)
           false)))))

(defn is-empty-dir?
  [dir]
  (let [fs (.listFiles dir)]
    (if (or (nil? fs) (zero? (alength fs)))
      true
      (let [ifns (get-default :os :ignorable-file-names)]
        (if (every? #(contains? ifns (.getName %)) (seq fs))
          true
          false)))))

(defn delete-dir?
  ([dir up-to]
     ;; Delete dir.
     (let [result (loop [fs (get-files-from-deepest dir)
                         r true]
                    (if (seq fs)
                      (let [f (first fs)
                            b (.delete f)]
                        (when-not b
                          (log-warning "delete-dir?: fail:" f))
                        (recur (rest fs) (and r b)))
                      r))]
       (delete-dir? dir up-to result)))
  ([dir up-to result]
     ;; Delete empty parent dir(s) up to up-to dir.
     (loop [pd (.getParentFile dir)]
       (when (and pd (not= pd up-to) (< (count (str up-to)) (count (str pd))))
         (when (is-empty-dir? pd)
           (.delete pd))
         (recur (.getParentFile pd))))
     result))

(defn trash-dir?
  [dir up-to]
  (if (.exists dir)
    (let [fs (get-files-from-deepest dir)]
      (loop [fs fs
             r true]
        (if (seq fs)
          (let [f (first fs)
                b (if (.isDirectory f)
                    (.delete f)
                    (trash-file? f))]
            (when-not b
              (log-warning "trash-dir: failed:" f))
            (recur (rest fs) (and r b)))
          (and r (delete-dir? dir up-to)))))
    (and true (delete-dir? dir up-to true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  [obj #^Long timeout & body]
  `(locking ~obj
     ~@body
     (.wait ~obj ~timeout)))

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
                (let [dst (File. (get-library-dir) (str file))]
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
;;;; startup/shutdown


(defn startup
  []
  (startup-or-shutdown :startup))

(defn shutdown
  []
  (startup-or-shutdown :shutdown))

;;;;

(defn get-sevenri-lock-file
  []
  (with-create-sn-get-dir
    (File. (get-sid-sevenri-dir) (str (get-default :sid :sevenri :lock-file-name)))))

(defn create-sevenri-lock-file?
  []
  (let [lf (get-sevenri-lock-file)]
    (if (.exists lf)
      false
      (do
        (spit lf "Don't disturb me")
        true))))

(defn remove-sevenri-lock-file?
  []
  (let [lf (get-sevenri-lock-file)]
    (when (.exists lf)
      (.delete lf))
    true))

;;;;

(defn create-sid-sevenri-dirs?
  []
  (get-sid-classes-dir)
  (get-sid-sevenri-dir)
  (get-sid-temp-dir)
  true)

(defn create-other-dirs?
  []
  (get-library-dir 'user)
  (get-temp-dir)
  true)

(defn aot-compile-sevenri-listeners?
  []
  (binding [*compile-path* (str (get-src-dir))]
    (compile (get-default :src :sevenri :listeners :aot))))

(defn setup-project-manager?
  []
  (when-let [pm (get-project-manager)]
    (when-not (query-project :ready? pm)
      (future
        (when-not (query-project :setup? pm)
          (log-severe "setup-project-manager?: failed to setup projet manager:" pm)))))
  true)

(defn shutdown-project-manager?
  []
  (when-let [pm (get-project-manager)]
    (query-project :shutdown pm))
  true)

;;;;

(defn startup-core?
  []
  (let [result (with-create-sn-get-dir
                 (and true
                      (create-sid-sevenri-dirs?)
                      (create-other-dirs?)
                      (aot-compile-sevenri-listeners?)
                      (create-sevenri-lock-file?)))]
    (and result
         (setup-project-manager?))))

(defn shutdown-core?
  []
  (and true
       (shutdown-project-manager?)
       (remove-sevenri-lock-file?)))
