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

(ns ^{:doc "Sevenri utility library"}
  sevenri.utils
  (:import (java.io File)
           (java.lang.reflect Field Modifier Method Constructor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def PHI (* 2 (Math/cos (/ Math/PI 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn elapsed-msecs
  [start end]
  (/ (double (- end start)) 1000000.0))

(defn expand-file-name
  "Converts filename to an absolute file. If directory is supplied, it is the
   default directory to start with if filename is relative. The value of
   directory should itself be an absolute directory file or absolute directory
   file name; it may start with '~'. Otherwise, the value of the system property
   'user.home' is used."
  ([filename]
     (expand-file-name filename (System/getProperty "user.home")))
  ([filename directory]
     (.getCanonicalFile
      (let [filename (str filename)
            first-char (first filename)
            fsep-char (.charAt (System/getProperty "file.separator") 0)
            user-home (System/getProperty "user.home")]
        (cond
         (= first-char fsep-char) (File. filename)
         (= first-char \~) (File. (File. user-home) (subs filename 1))
         :else (File. (File. (let [directory (str directory)
                                   first-char (first directory)]
                               (cond
                                (= first-char fsep-char) directory
                                (= first-char \~) (str user-home (subs directory 1))
                                :else (throw (IllegalArgumentException.
                                              (str "invalid directory: " directory))))))
                      filename))))))

(defn find-files
  "Returns a lazy seq of the files on path for which (pred file) returns true.
  pred must take a Java file object and be free of side-effects. For convenience
  pred can also be either true or an extension symbol, like '.clj. When true,
  return all files found in path. When an extension symbol, return all files
  with that extension."
  [pred path]
  (let [fsq (file-seq (if (instance? File path) path (File. path)))
        prd (cond
             (true? pred)
               #(when (.exists %)
                  (identity %))
             (and (symbol? pred) (= (first (name pred)) \.))
               #(re-find (re-pattern (str \\ pred \$)) (str %))
             :else
               pred)]
    (when (fn? prd)
      (filter prd fsq))))

(defn print-seq
  "A generic seq printer"
  [sq]
  (doseq [i (seq sq)] (printf "%s\n" i)))

(defn System-properties
  ([]
         (System-properties ".*"))
  ([^String rgex]
         (filter #(re-find (re-pattern rgex) (str %)) (seq (System/getProperties)))))

(defn classpath
  "Returns a sequence of File objects on CLASSPATH.
  Running test with this function prints non-existing local paths,
  if any, to *err*."
  {:test (fn []
           (binding [*out* *err*]
             (dorun (map #(when (not (.exists (File. (.getFile %))))
                            (println (str %)))
                         (filter #(if (= (.getProtocol %) "file") %) (classpath))))))}
  []
  (seq (.getURLs (ClassLoader/getSystemClassLoader))))

(defn reflect
  ;; From: http://groups.google.com/group/clojure/msg/96ed91f823305f02
  "usage:
   (reflect Object)   ; give it a class
   (reflect Object 1) ; a class and a method number to see details
   (reflect {})       ; or give it an instance"
  ([x] (reflect x nil))
  ([x i]
     (let [c (if (class? x) x (class x))
           items (sort
                  (for [m (concat (.getDeclaredFields c)
                                  (.getDeclaredMethods c)
                                  (.getDeclaredConstructors c))]
                    (let [static? (bit-and Modifier/STATIC (.getModifiers m))
                          method? (instance? Method m)
                          ctor?   (instance? Constructor m)
                          grt     (fn [m]
                                    (if (.isArray (.getReturnType m))
                                      (str (.getName (.getComponentType (.getReturnType m))) [])
                                      (.getName (.getReturnType m))))
                          gpt     (fn [p]
                                    (if (.isArray p)
                                      (str (.getName (.getComponentType p)) [])
                                      (.getName p)))
                          text    (if ctor?
                                    (str "CTOR (" (apply str (interpose ", " (map gpt (.getParameterTypes m)))) ")")
                                    (str
                                     (when (not method?) "FIELD ")
                                     (.getName m) " : "
                                     (when (pos? static?) "static ")
                                     (if method?
                                       (str (grt m)
                                            " (" (apply str (interpose ", " (map gpt (.getParameterTypes m)))) ")"
                                            (when (pos? (count (.getExceptionTypes m)))
                                              " Throws"))
                                       (str (gpt (.getType m))))))]
                      [method? ctor? text (str m) m])))]
       (if i
         (last (nth items i))
         (do (println "===" c (str (if (.getSuperclass c) (str ": " (.getName (.getSuperclass c))))) "===")
             (doseq [[e i] (map list items (iterate inc 0))]
               (printf "[%3d] %s\n" i (nth e 2))))))))

(defn copy-ext-files
  [srcdir tgtdir ext]
  (let [srcdir (if (= (class srcdir) java.io.File) srcdir (File. srcdir))
        tgtdir (if (= (class tgtdir) java.io.File) tgtdir (File. tgtdir))]
    (let [sfs (find-files ext srcdir)
          regx (re-pattern (format "%s/(.*).%s$" srcdir ext))
          stfs (map #(let [s (File. (second (re-find regx (str %))))]
                       [% (.getParent s) (.getName s)])
                    sfs)]
      (doseq [[sf d f] stfs]
        (let [td (File. tgtdir d)
              tf (File. td (str f \. ext))]
          (.mkdirs td)
          (clojure.java.io/copy sf tf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

(def -unsafe-chars- "[*?:|<>\"\\\\\\s]") ;; * ? : | < > " \ \s
(def -sym2path-proxy- "_")
(def -path2sym-proxy- "-")

(defn nssym2path
  [nssym]
  (-> (str nssym)
      (.replace \- \_)
      (.replace \. \/)
      (.replace \! \.)
      (.replaceAll -unsafe-chars- -sym2path-proxy-)))

(defn path2nssym
  [path]
  (-> (str path)
      (.replace \. \!)
      (.replace \/ \.)
      (.replace \_ \-)
      (.replaceAll -unsafe-chars- -path2sym-proxy-)
      (symbol)))
