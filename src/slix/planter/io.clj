(ns slix.planter.io
  (:use [sevenri config core log slix utils])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-all-project-config-files
  []
  (find-files #(= "project.clj" (.getName %)) (get-project-dir)))

(defn get-project-name-config-map
  "map of name as key and config as value"
  []
  (let [projpath (str (get-project-dir))
        projplen (inc (count projpath))
        projflen (count "/project.clj")]
    (letfn [(fton [f]
              (let [fullpath (str f)
                    fpathlen (count fullpath)
                    subfpath (.substring fullpath projplen (- fpathlen projflen))]
                (list (path2nssym subfpath) f)))]
      (apply hash-map (flatten (map fton (get-all-project-config-files)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-project-name
  [slix proj-name]
  (with-create-sn-get-dir
    (let [sf (get-sid-slix-state-file slix)]
      (spit sf (str proj-name) :encoding "UTF-8"))))

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

