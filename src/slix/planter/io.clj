(ns slix.planter.io
  (:use [clojure.java.shell]
        [sevenri config core log slix utils])
  (:import (java.io ByteArrayOutputStream PrintStream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-all-project-config-files
  []
  (find-files #(= "project.clj" (.getName %)) (get-project-path)))

(defn get-project-name-config-map
  "map of name as key and config as value"
  []
  (let [projpath (str (get-project-path))
        projplen (inc (count projpath))
        projflen (count "/project.clj")]
    (letfn [(fton [f]
              (let [fullpath (str f)
                    fpathlen (count fullpath)
                    subfpath (.substring fullpath projplen (- fpathlen projflen))]
                (list (path2sym subfpath) f)))]
      (apply hash-map (flatten (map fton (get-all-project-config-files)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-project-name
  [slix proj-name]
  (let [sf (get-sid-slix-state-file slix)]
    (spit sf (str proj-name) :encoding "UTF-8")))

(defn save-state
  [slix proj-name]
  (save-project-name slix proj-name))

(defn load-project-name
  [slix]
  (let [sf (get-sid-slix-state-file slix)]
    (when (.exists sf)
      (let [pn (slurp sf :encoding "UTF-8")]
        (if (empty? pn)
          nil
          (symbol pn))))))

(defn load-state
  [slix]
  (when-let [pn (load-project-name slix)]
    {:project pn}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-out-ps
  []
  (let [baos (ByteArrayOutputStream.)
        oprs (PrintStream. baos true)]
    [baos oprs]))

(defn do-git-clone
  [dir repo-url]
  (try
    (with-sh-dir dir
      (sh "git" "clone" repo-url))
    (catch Exception e
      {:exit 1 :out "" :err ("low level git-clone fn failed:" repo-url)})))
