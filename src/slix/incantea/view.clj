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

(in-ns 'slix.incantea.ui)

(use '[slix.incantea defs listeners])
(import '(java.awt BorderLayout)
        '(java.util Vector)
        '(javax.swing JInternalFrame JPanel)
        '(javax.swing ImageIcon JFrame JLabel JSlider JScrollPane JTable)
        '(javax.swing.event ChangeEvent ChangeListener)
        '(org.jfree.chart ChartFrame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-view-by
  ([label get-label-fn]
     (when-let [desktop (get-desktop)]
       (get-view-by label get-label-fn desktop)))
  ([label get-label-fn desktop]
     (when (and (or (symbol? label) (string? label))
                (fn? get-label-fn)
                (instance? JDesktopPane desktop))
       (declare get-views)
       (filter #(= (str label) (get-label-fn %)) (get-views desktop false)))))

(defn get-view-by-title
  [title]
  (get-view-by title #(.getTitle %)))

(defn get-view-by-name
  [name]
  (get-view-by name #(.getName %)))

(defmulti get-view
  "object is either a view object or view name in symbol or string. Search
   view with the matching name or identical to the given one in the list of
   existing views and return it if it is found. Return nil otherwise."
  (fn [object]
    (cond
     (or (symbol? object) (string? object)) :name
     (instance? JInternalFrame object) :view
     :else :default)))

(defmethod get-view :name
  [name]
  (get-view-by-name name))

(defmethod get-view :view
  [view]
  (when (some #(identical? view %) (get-views))
    view))

(defmethod get-view :default
  [object]
  nil)

(defn get-sorted-views
  [views]
  (sort *view-comparator* views))

(defn get-views
  ([]
     (when-let [desktop (get-desktop)]
       (get-views desktop)))
  ([desktop]
     (get-views desktop false))
  ([desktop sort?]
     (when (instance? JDesktopPane desktop)
       (when-let [rfvws (.getClientProperty desktop *prop-ref-views*)]
         (when-not (empty? @rfvws)
           (if sort?
             (get-sorted-views @rfvws)
             @rfvws))))))

(defn generate-view-name
  ([views]
     (generate-view-name views "view"))
  ([views base]
     (generate-view-name views (if (empty? base) "view" base) nil))
  ([views base infix]
     (generate-view-name views base infix 0))
  ([views base infix postnum]
     (let [view-names (apply hash-set (map #(.getName %) views))]
       (loop [name base
              pnum postnum]
         (if (get view-names name)
           (recur (str base infix (inc pnum)) (inc pnum))
           name)))))

(defn -register-view
  ([view]
     (when-let [desktop (get-desktop)]
       (-register-view view desktop)))
  ([view desktop]
     (when (and (instance? JInternalFrame view)
                (instance? JDesktopPane desktop))
       (dosync
        (let [rfvws (or (.getClientProperty desktop *prop-ref-views*) (ref nil))
              vname (generate-view-name @rfvws (.getTitle view))]
          (.putClientProperty desktop *prop-ref-views* rfvws)
          (doto view
            (.setName vname)
            (.setTitle vname))
          (ref-set rfvws (conj @rfvws view)))))))

(defn -unregister-view
  ([view]
     (when-let [desktop (get-desktop)]
       (-unregister-view view desktop)))
  ([view desktop]
     (when (and (instance? JInternalFrame view)
                (instance? JDesktopPane desktop))
       (dosync
        (let [rfvws (.getClientProperty desktop *prop-ref-views*)
              views (filter #(not= view %) @rfvws)]
          (ref-set rfvws views)
          views)))))

(defn- -add-view
  [view]
  (let [dsktp (get-desktop)
        vacts (:vactions (-get-main-controls))]
    (doto view
      (add-key-listener dsktp get-views -get-main-controls)
      (add-view-listener dsktp vacts -register-view -unregister-view)
      (.setVisible true))
    (.add dsktp view)
    view))

(defn get-views-action-keyword
  []
  (keys *views-items*))

(defn do-views
  [action-keyword]
  (when (and (keyword? action-keyword)
             (get-desktop))
    (invoke-later #(dispatch-views-action action-keyword (get-desktop) get-views))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -add-sketch
  [frame sketch]
  (if-let [desktop (get-desktop)]
    (do
      (dosync
       (let [rfskcs (or (.getClientProperty desktop *prop-ref-sketches*) (ref nil))
             sfname (generate-view-name @rfskcs (.getTitle frame))]
         (.putClientProperty desktop *prop-ref-sketches* rfskcs)
         (doto frame
           (.setName sfname)
           (.setTitle sfname))
         (ref-set rfskcs (conj @rfskcs frame))))
      (doto frame
        (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
        (.addWindowListener (proxy [WindowAdapter] []
                              (windowClosing [e]
                                (.destroy sketch)
                                (.dispose frame)
                                (declare remove-sketch)
                                (remove-sketch frame desktop))))
        (.setVisible true))
      frame))
  (do
    (.destory sketch)
    (.dispose frame)
    (log-severe "incantea: -add-sketch: get-desktop failed")))

(defn remove-sketch
  ([frame]
     (when (instance? JFrame frame)
       (when-let [desktop (get-desktop)]
         (remove-sketch frame desktop))))
  ([frame desktop]
     (dosync
      (let [rfskcs (.getClientProperty desktop *prop-ref-sketches*)
            frames (filter #(not (identical? frame %)) @rfskcs)]
        (.destroy (.getComponent (.getComponentPane frame) 0))
        (.dispose frame)
        (ref-set rfskcs frames)))))

(defn remove-all-sketches
  []
  (when-let [desktop (get-desktop)]
    (when-let [rfskcs (.getClientProperty desktop *prop-ref-sketches*)]
      (doseq [frame @rfskcs]
        (remove-sketch frame desktop)))))

(defn get-sketches
  []
  (when-let [desktop (get-desktop)]
    (when-let [rfskcs (.getClientProperty desktop *prop-ref-sketches*)]
      @rfskcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following code is part of incanter modified for incantea.

;; Copyright (c) David Edgar Liebke, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.htincanter.at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VIEW METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti view
  " This is a general 'view' function. When given an Incanter matrix/dataset
    or a Clojure numeric collection, it will display it in a Java Swing
    JTable. When given an Incanter chart object, it will display it in a new
    window. When given a URL string, it will open the location with the
    platform's default web browser.

    When viewing charts, a :width (default 500) and :height (default 400) 
    option can be provided.

    When viewing an incanter.processing sketch, set the :exit-on-close option
    to true (default is false) to kill the animation processes when you
    close the window (this will also kill your REPL or Swank server), 
    otherwise those processing will continue to run in the background.

    Examples:

      (use-incanter)

      ;; view matrices
      (def rand-mat (matrix (sample-normal 100) 4))
      (view rand-mat)

      ;; view numeric collections
      (view [1 2 3 4 5])
      (view (sample-normal 100))

      ;; view Incanter datasets
      (view (get-dataset :iris))

      ;; convert dataset to matrix, changing Species names to numeric codes
      (view (to-matrix (get-dataset :iris)))

      ;; convert dataset to matrix, changing Species names to dummy variables
      (view (to-matrix (get-dataset :iris) :dummies true))

      ;; view a chart
      (view (histogram (sample-normal 1000)) :width 700 :height 700)

      ;; view a URL
      (view \"http://incanter.org\")

      ;; view a PNG file
      (save (histogram (sample-normal 1000)) \"/tmp/norm_hist.png\")
      (view \"file:///tmp/norm_hist.png\")
"
  (fn [obj & options] (cond
                        (and (not (matrix? obj))
                             (not (dataset? obj))
                             (not (map? obj))
                             (coll? obj))
                          ::coll
                        (.contains (str (type obj)) "processing.core.PApplet")
                          :sketch
                        :else
                        (type obj))))

(defmethod view ::coll
  ([obj & options]
     (let [rows (if (coll? (first obj))
                  obj
                  (map vector obj))
           colnames (range (count (first rows)))]
       (view (dataset colnames rows)))))

(defmethod view incanter.Matrix
  ([obj & options]
     (let [view (JInternalFrame. "Matrix" true true true true)]
       (invoke-later
        (fn []
          (let [opts (when options (apply assoc {} options))
                col-names (or (:column-names opts) (range (ncol obj)))
                m (ncol obj)
                n (nrow obj)
                table (JTable.
                       (cond
                        (and (> m 1) (> n 1))
                          (Vector. (map #(Vector. %) (to-list obj)))
                        (or (and (> m 1) (= n 1)) (and (= m 1) (= n 1)))
                          (Vector. (map #(Vector. %) [(to-list obj) []]))
                        (and (= m 1) (> n 1))
                          (Vector. (map #(Vector. [%]) (to-list obj))))
                       (Vector. col-names))]
            (add-key-listener table (get-desktop) get-views -get-main-controls)
            (doto view
              (.add (JScrollPane. table))
              (.setSize 400 600)
              (-add-view)))))
       view)))

(defmethod view :incanter.core/dataset
  ([obj & options]
     (let [view (JInternalFrame. "Dataset" true true true true)]
       (invoke-later
        (fn []
          (let [col-names (:column-names obj)
                column-vals (map (fn [row] (map #(row %) col-names)) (:rows obj))
                table (JTable. (Vector. (map #(Vector. %) column-vals))
                               (Vector. col-names))]
            (add-key-listener table (get-desktop) get-views -get-main-controls)
            (doto view
              (.add (JScrollPane. table))
              (.setSize 400 600)
              (-add-view)))))
       view)))

(defmethod view javax.swing.JTable
  ([obj & options]
     (let [view (JInternalFrame. "Dataset" true true true true)]
       (invoke-later
        (fn []
          (add-default-key-listener obj)
          (doto view
            (.add (JScrollPane. obj))
            (.setSize 500 600)
            (-add-view))))
       view)))

(defmethod view java.awt.Image
  ([obj & options]
     (let [view (JInternalFrame. "Image" true true true true)]
       (invoke-later
        (fn []
          (let [icon (ImageIcon. obj)
                label (JLabel. icon)
                scrlp (JScrollPane. label)
                height (+ 15 (.getIconHeight icon))
                width (+ 15 (.getIconWidth icon))]
            (doto label
              (.setFocusable true)
              (add-key-listener (get-desktop) get-views -get-main-controls))
            (doto view
              (.add scrlp)
              (.setSize height width)
              (-add-view)))))
       view)))

;; URL view method code lifted from clojure.contrib.javadoc.browse/open-url-in-browser
(defmethod view String
  ([url]
    (try
      (when (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" "isDesktopSupported" (to-array nil))
        (-> (clojure.lang.Reflector/invokeStaticMethod "java.awt.Desktop" "getDesktop" (to-array nil))
            (.browse (java.net.URI. url)))
        url)
      (catch ClassNotFoundException e nil))))

(defmethod view org.jfree.chart.JFreeChart
  ([chart & options]
     (let [opts (when options (apply assoc {} options))
           wttl (or (:window-title opts) "Plot")
           view (JInternalFrame. wttl true true true true)]
       (invoke-later
        (fn []
          (let [width (or (:width opts) 500)
                height (or (:height opts) 400)
                frame (ChartFrame. wttl chart)
                cpanel (.getChartPanel frame)]
            (add-key-listener cpanel (get-desktop) get-views -get-main-controls)
            (doto view
              (.setContentPane cpanel)
              (.setSize width height)
              (-add-view)))))
       view)))

(defmethod view :sketch
  ([sketch & options]
     (let [opts (when options (apply assoc {} options))
           wttl (or (:title opts) (str (get-spell-name) " - Processing Sketch"))
           sfrm (JFrame. wttl)]
       (invoke-later
        (fn []
          (let [width (or (:width opts) (.width (.getSize sketch)))
                height (or (:height opts) (+ 22 (.height (.getSize sketch))))
                [width height] (or (:size opts)
                                   [(.width (.getSize sketch))
                                    (.height (.getSize sketch))])]
            (.add (.getContentPane sfrm) sketch)
            (doto sfrm
              (add-default-key-listener)
              (.setSize width height)
              (-add-sketch sketch)))))
       sfrm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIDER CONTROLS AND PLOTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn slider
  " Examples:

    (use-incanter)

    (def pdf-chart (function-plot pdf-normal -3 3))
    (view pdf-chart)
    (add-function pdf-chart pdf-normal -3 3)

    (let [x (range -3 3 0.1)] 
      (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1)))

    (let [x (range -3 3 0.1)] 
      (slider #(set-data pdf-chart [x (pdf-normal x :sd %)]) (range 0.1 10 0.1) \"sd\"))
"
  ([updater-fn slider-values]
     (slider updater-fn slider-values nil))
  ([updater-fn slider-values slider-label]
     (let [view (JInternalFrame. (generate-view-name "Slider Control")
                                 true true true true)]
       (invoke-later
        (fn []
          (let [max-idx (count slider-values)
                label-txt (fn [v]
                            (str (when slider-label
                                   (str slider-label " = ")) v))
                label (JLabel. (label-txt (first slider-values)) JLabel/CENTER)
                slider (doto (JSlider. JSlider/HORIZONTAL 0 max-idx 0)
                         (add-default-key-listener)
                         (.addChangeListener (proxy [ChangeListener] []
                                               (stateChanged
                                                 [^ChangeEvent event]
                                                 (let [source (.getSource event)
                                                       value (nth slider-values (.getValue source))]
                                                   (do
                                                     (.setText label (label-txt value))
                                                     (updater-fn value)))))))
                panel (doto (JPanel. (BorderLayout.))
                        (.add label BorderLayout/NORTH)
                        (.add slider BorderLayout/CENTER))
                width 500
                height 80]
            (add-key-listener panel (get-desktop) get-views -get-main-controls)
            (doto view
              (.add panel BorderLayout/CENTER)
              (.setSize width height)
              (-add-view)))))
       view)))

(defn sliders*
  " sliders*

    Examples:

      (use-incanter)

      (let [x (range -3 3 0.1)] 
        (do
          (def pdf-chart (xy-plot x (pdf-normal x :mean -3 :sd 0.1)))
          (view pdf-chart)
          (sliders* #(set-data pdf-chart [x (pdf-normal x :mean %1 :sd %2)]) 
                    [(range -3 3 0.1) (range 0.1 10 0.1)] 
                    [\"mean\" \"sd\"])))
"
  ([f [& slider-values]]
     (sliders* f (apply vector slider-values) [nil]))
  ([f [& slider-values] [& slider-labels]]
     (let [init-values (map first slider-values)
           refs (map ref init-values)
           slider-fns (map #(fn [v] 
                              (do 
                                (dosync (ref-set (nth refs %) v)) 
                                (apply f (map deref refs))))
                           (range (count refs)))
           _ ((first slider-fns) (first init-values))]
       (if slider-labels 
         (map slider slider-fns slider-values slider-labels)
         (map slider slider-fns slider-values)))))

(defmacro sliders
  " Creates one slider control for each of the given sequence bindings.
    Each slider calls the given expression when manipulated. 

    Examples:

      (use-incanter)
  
      ;; manipulate a normal pdf
      (let [x (range -3 3 0.1)]
        (def pdf-chart (xy-plot))
        (view pdf-chart) 
        (sliders [mean (range -3 3 0.1) 
                  stdev (range 0.1 10 0.1)]
          (set-data pdf-chart [x (pdf-normal x :mean mean :sd stdev)])))

      ;; manipulate a gamma pdf
      (let [x (range 0 20 0.1)]
        (def pdf-chart (xy-plot))
        (view pdf-chart) 
        (sliders [rate (range 0.1 10 0.1) 
                  shape (range 0.1 10 0.1)]
          (set-data pdf-chart [x (pdf-gamma x :rate rate :shape shape)])))

      ;; find the start values of a non-linear model function
      (use-incanter)
      ;; create model function used in the following data-sorcery post:
      ;; http://data-sorcery.org/2009/06/06/fitting-non-linear-models/

      (defn f [theta x]
        (let [[b1 b2 b3] theta]
          (div (exp (mult (minus b1) x)) (plus b2 (mult b3 x)))))

      (with-data (get-dataset :chwirut)
        (view $data)
        (def chart (scatter-plot ($ :x) ($ :y)))
        (view chart)
        (add-lines chart ($ :x) (f [0 0.01 0] ($ :x)))

      ;; manipulate the model line to find some good start values.
      ;; give the index of the line data (i.e. 1) to set-data.
      (let [x ($ :x)] 
        (sliders [b1 (range 0 2 0.01)
                  b2 (range 0.01 2 0.01)
                  b3 (range 0 2 0.01)]
          (set-data chart [x (f [b1 b2 b3] x)] 1))))
"
  ([[& slider-bindings] body]
     `(let [slider-fn# (fn ~(apply vector (map symbol (take-nth 2 slider-bindings))) 
                         (do ~body))
            slider-labels# ~(apply vector (map str (take-nth 2 slider-bindings)))]
        (sliders* slider-fn# ~(apply vector (take-nth 2 (rest slider-bindings))) slider-labels#))))

(defmacro dynamic-xy-plot
  " Returns an xy-plot bound to sliders (which tend to appear behind the chart). 
    See the sliders macro for more information.

    Examples:

      (use-incanter)

      (let [x (range -3 3 0.1)]
        (view (dynamic-xy-plot [mean (range -3 3 0.1)
                                sd (range 0.1 10 0.1)]
                [x (pdf-normal x :mean mean :sd sd)]
                :title \"Normal PDF Plot\")))

       (let [x (range -3 3 0.1)]
         (view (dynamic-xy-plot [mean (range -3 3 0.1)
                                 sd (range 0.1 10 0.1)]
                (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
                :title \"Normal PDF Plot\")))
"
  ([[& slider-bindings] expression & options]
     `(let [chart# (xy-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))

(defmacro dynamic-scatter-plot
  " Returns an scatter-plot bound to sliders (which tend to appear behind the chart). 
    See the sliders macro for more information.

    Examples:

      (use-incanter)

      (let [x (range -3 3 0.1)]
        (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                     sd (range 0.1 10 0.1)]
                [x (pdf-normal x :mean mean :sd sd)]
                :title \"Normal PDF Plot\")))

       (let [x (range -3 3 0.1)]
         (view (dynamic-scatter-plot [mean (range -3 3 0.1)
                                      sd (range 0.1 10 0.1)]
                (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
                :title \"Normal PDF Plot\")))
"
  ([[& slider-bindings] expression & options]
     `(let [chart# (scatter-plot [] [] ~@options)
            sliders# (sliders ~(vec slider-bindings)
                              (set-data chart# ~expression))]
        (doall sliders#)
        (set-x-label chart# (str '~(first expression)))
        (set-y-label chart# (str '~(second expression))))))
