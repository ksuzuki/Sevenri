(ns slix.planter.io
  (:use [sevenri config core log utils])
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
