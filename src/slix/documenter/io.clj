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

(ns slix.documenter.io
  (:use [sevenri config core log slix utils])
  (:import (java.io File)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; doc
;;   manuals
;;     .metadoc
;;     Sevenri_Users_Manual
;;       .metadoc
;;       src
;;         .metadoc
;;         Table_of_Contents.md
;;         Section1.md
;;         Section2.md
;;          :
;;       out
;;         .metadoc
;;         Table_of_Contents.html
;;         Section1.html
;;         Section2.html
;;          :
;;       res
;;         figure1.jpg
;;         figure2.png
;;

(def *metadoc-file-name* ".metadoc")
(def *prop-section-file* "section-file")

(def *src-itm* "src")
(def *src-ext* ".md")
(def *out-itm* "out")
(def *out-ext* ".html")
(def *res-itm* "res")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-metadoc-file
  [dir]
  (File. dir *metadoc-file-name*))

(defn read-metadoc
  [dir-or-file]
  (let [mtdc (if (.isDirectory dir-or-file)
                  (get-metadoc-file dir-or-file)
               dir-or-file)]
    (when (.exists mtdc)
      (read-string (slurp mtdc)))))

(defn write-metadoc
  ([dir mdata]
     (write-metadoc dir mdata false))
  ([dir mdata reset?]
     (let [new-mdata (if reset?
                       mdata
                       (merge (read-metadoc dir) mdata))
           mtdc (get-metadoc-file dir)]
       (spit mtdc new-mdata)
       true)))

;;;;

(defn read-root-metadoc
  []
  (read-metadoc (get-doc-path)))

(defn write-root-metadoc
  ([mdata]
     (write-metadoc (get-doc-path) mdata))
  ([mdata reset?]
     (write-metadoc (get-doc-path) mdata reset?)))

(defn update-metadocs
  [dir mdata]
  (doseq [mtdcf (find-files #(= (.getName %) *metadoc-file-name*) dir)]
    (write-metadoc (.getParentFile mtdcf) mdata))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-category
  [category]
  (when (string? category)
    (let [cdir (with-make-path (get-doc-path (symbol category)))
          mtdc (read-metadoc cdir)]
      (when cdir
        (write-metadoc cdir (merge mtdc {:category category}))
        ;;
        (let [rmtdc (read-root-metadoc)
              ctgrs (set (:categories rmtdc))]
          (write-root-metadoc (merge rmtdc {:categories (conj ctgrs category)})))
        ;;
        cdir))))

(defn get-category
  [category]
  (let [rmtdc (read-root-metadoc)]
    (when (and (string? category) (contains? (:categories rmtdc) category))
      (let [cdir (get-doc-path (symbol category))]
        (when (and (.isDirectory cdir)
                   (= (:category (read-metadoc cdir)) category))
          cdir)))))

(defn remove-category
  [category]
  (when-let [cdir (get-category category)]
    (when (clean-path? cdir (get-doc-path))
      (let [rmtdc (read-root-metadoc)
            ctgrs (:categories rmtdc)]
        (write-root-metadoc (merge rmtdc {:categories (disj ctgrs category)}))))))

(defn rename-category
  "'from' should exist and 'to' shouldn't."
  [from to]
  (when (every? #(string? %) [from to])
    (when-not (get-category to)
      (when-let [from-cdir (get-category from)]
        (let [to-cdir (File. (.getParentFile from-cdir) (sym2path to))]
          (when (.renameTo from-cdir to-cdir)
            (let [rmtdc (read-root-metadoc)
                  ctgrs (:categories rmtdc)]
              (write-root-metadoc (merge rmtdc {:categories (conj (disj ctgrs from) to)})))
            (update-metadocs to-cdir {:category to})
            to-cdir))))))

;;;;

(defn add-title
  [title category]
  (when (every? #(string? %) [title category])
    (when-let [cdir (add-category category)]
      (let [tdir (File. cdir (sym2path title))]
        (when-not (.exists tdir)
          (.mkdir tdir))
        (let [mtdc (read-metadoc tdir)]
          (write-metadoc tdir (merge mtdc {:title title :category category}))
          tdir)))))
      
(defn get-title
  [title category]
  (when (every? #(string? %) [title category])
    (when-let [cdir (get-category category)]
      (let [tdir (File. cdir (sym2path title))
            mtdc (read-metadoc tdir)]
        (when (and (.isDirectory tdir)
                   (= (:title mtdc) title)
                   (= (:category mtdc) category))
          tdir)))))

(defn remove-title
  [title category]
  (when-let [tdir (get-title title category)]
    (let [cdir (get-category category)]
      (clean-path? tdir cdir))))

(defn rename-title
  "'from' should exist and 'to' shouldn't."
  [from to category]
  (when (every? #(string? %) [from to category])
    (when-not (get-title to category)
      (when-let [from-tdir (get-title from category)]
        (let [to-tdir (File. (.getParentFile from-tdir) (sym2path to))]
          (when (.renameTo from-tdir to-tdir)
            (update-metadocs to-tdir {:title to})
            to-tdir))))))

;;;;

(defn add-item
  [item title category]
  (when (every? #(string? %) [item title category])
    (when-let [tdir (add-title title category)]
      (let [idir (File. tdir (sym2path item))]
        (when-not (.exists idir)
          (.mkdir idir))
        idir))))

(defn get-item
  [item title category]
  (when (every? #(string? %) [item title category])
    (when-let [tdir (get-title title category)]
      (let [idir (File. tdir (sym2path item))]
        (when (.isDirectory idir)
          idir)))))

(defn remove-item
  [item title category]
  (when-let [idir (get-item item title category)]
    (let [tdir (get-title title category)]
      (clean-path? idir tdir))))

;;;;

(defn add-item-section
  [section ext item title category]
  (when (every? #(string? %) [section ext item title category])
    (when-let [sdir (add-item item title category)]
      (let [mtdc (read-metadoc sdir)
            sfnm (str (sym2path section) ext)
            secf (File. sdir sfnm)]
        (when-not (.exists secf)
          (.createNewFile secf))
        (let [sections (if (:sections mtdc)
                         (merge (:sections mtdc) section)
                         (hash-set section))]
          (write-metadoc sdir (merge mtdc {:sections sections :title title :category category}))
        secf)))))

(defn get-item-section
  [section ext item title category]
  (when (every? #(string? %) [section ext item title category])
    (when-let [sdir (get-item item title category)]
      (let [mtdc (read-metadoc sdir)
            sfnm (str (sym2path section) ext)
            secf (File. sdir sfnm)]
        (when (and (.isFile secf)
                   (contains? (:sections mtdc) section)
                   (= (:title mtdc) title)
                   (= (:category mtdc) category))
          secf)))))

(defn remove-item-section
  [section ext item title category]
  (when-let [secf (get-item-section section ext item title category)]
    (trash-path? secf)
    (let [sdir (get-item item title category)
          mtdc (read-metadoc sdir)
          scts (disj (:sections mtdc) section)]
      (write-metadoc sdir
                     (if (seq scts)
                       (merge mtdc {:sections scts})
                       (dissoc mtdc :sections))
                     true))))

(defn rename-item-section
  [from to ext item title category]
  (when (every? #(string? %) [from to ext item title category])
    (when-not (get-item-section to ext item title category)
      (when-let [from-secf (get-item-section from ext item title category)]
        (let [to-secf (File. (.getParentFile from-secf) (str (sym2path to) ext))]
          (when (.renameTo from-secf to-secf)
            (let [idir (.getParentFile to-secf)
                  mtdc (read-metadoc idir)
                  scts (:sections mtdc)]
              (write-metadoc idir {:sections (conj (disj scts from) to)})
              to-secf)))))))

;;;;

(defn add-src-section
  [section title category]
  (add-item-section section *src-ext* *src-itm* title category))

(defn get-src-section
  [section title category]
  (get-item-section section *src-ext* *src-itm* title category))

(defn remove-src-section
  [section title category]
  (remove-item-section section *src-ext* *src-itm* title category))

(defn rename-src-section
  [from to title category]
  (rename-item-section from to *src-ext* *src-itm* title category))

;;;;

(defn add-out-section
  [section title category]
  (add-item-section section *out-ext* *out-itm* title category))

(defn get-out-section
  [section title category]
  (get-item-section section *out-ext* *out-itm* title category))

(defn remove-out-section
  [section title category]
  (remove-item-section section *out-ext* *out-itm* title category))

(defn rename-out-section
  [from to title category]
  (rename-item-section from to *out-ext* *out-itm* title category))

;;;;

(defn add-res
  [title category]
  (add-item *res-itm* title category))

(defn get-res
  [title category]
  (get-item *res-itm* title category))

(defn remove-tes
  [title category]
  (remove-item *res-itm* title category))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn write-section
  ([textpane]
     (write-section textpane false))
  ([textpane clear?]
     (when (.isEditable textpane)
       (when-let [file (.getClientProperty textpane *prop-section-file*)]
         (if (trash-path? file)
           (do
             (spit file (.getText textpane) :encoding "UTF-8")
             (when clear?
               (doto textpane
                 (.putClientProperty *prop-section-file* nil)
                 (.setText ""))))
           (log-warning "documenter: trash-path failed:" file))))))

(defn read-section
  [textpane file]
  (write-section textpane true)
  (when (.exists file)
    (doto textpane
      (.setText (slurp file :encoding "UTF-8"))
      (.setCaretPosition 0)
      (.putClientProperty *prop-section-file* file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-current-section-title-category
  [slix section-title-category]
  (let [[frame-file state-file] (slix-file-bundle slix)
          cont (or section-title-category [])]
      (spit state-file cont :encoding "UTF-8")))

(defn load-last-section-title-category
  [slix]
  (let [[frame-file state-file] (slix-file-bundle slix)]
    (when (.exists state-file)
      (try
        (read-string (slurp state-file :encoding "UTF-8"))
        (catch Exception e
          (log-warning "documenter: load-last-section-title-category failed:" state-file)
          nil)))))
