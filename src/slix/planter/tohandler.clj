(ns slix.planter.tohandler
  (:use [sevenri log slix]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def re-xml-header #"^\s*<\?xml\s.*\?>\s*$")
(def re-beg-xml-element #"^\s*<[^<>]+>\s*(\S.*)*$")
(def re-end-xml-element #"^([^<>]+)<.*>\s*$")

(def re-beg-testsuite #"\s*<testsuite\s+.*")
(def re-beg-failure #"^\s*<failure\s+[^>]+>\s*(\S.*)*$")
(def re-beg-error #"^\s*<error\s+[^>]+>\s*(\S.*)*$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-test-output-context
  [slix ins warn-attr]
  {:slix slix :ins ins :atr nil :watr warn-attr :output ""})

(defmulti handle-test-output
  (fn [output-line toctxt]
    (cond
     (re-matches re-xml-header output-line) :xml-header
     (re-matches re-beg-xml-element output-line) :beg-xml-element
     (re-matches re-end-xml-element output-line) :end-xml-element
     :else :default)))

(defmethod handle-test-output :xml-header
  [xml-header toctxt]
  (if (:xml-header toctxt)
    toctxt
    (assoc toctxt
           :xml-header true
           :output (str (:output toctxt) xml-header "\n"))))

(defmethod handle-test-output :beg-xml-element
  [xml-element toctxt]
  (if (re-matches re-beg-testsuite xml-element)
    (let [pkg (second (re-find #"package=\"([^\"]*)\"" xml-element))
          nme (second (re-find #"name=\"([^\"]*)\"" xml-element))
          msg (format "%sTesting %s.%s\n" (if (:testing toctxt) "\n" "") pkg nme)]
      (when-not (empty? pkg)
        (invoke-later (:slix toctxt) #((:ins toctxt) msg)))
      (assoc toctxt
        :testing true
        :output (str (:output toctxt) xml-element "\n")))
    ;;
    (let [s (second (re-matches re-beg-xml-element xml-element))
          w (or (re-matches re-beg-failure xml-element)
                 (re-matches re-beg-error xml-element))
          a (when w (:watr toctxt))]
      (when s
        (invoke-later (:slix toctxt) #((:ins toctxt) (str s "\n") a)))
      (assoc toctxt
        :atr a
        :output (str (:output toctxt) xml-element "\n")))))

(defmethod handle-test-output :end-xml-element
  [xml-element toctxt]
  (let [s (second (re-matches re-end-xml-element xml-element))]
    (when s
      (invoke-later (:slix toctxt) #((:ins toctxt) (str s "\n") (:atr toctxt))))
    (assoc toctxt
      :atr nil
      :output (str (:output toctxt) xml-element "\n"))))

(defmethod handle-test-output :default
  [output-line toctxt]
  (invoke-later (:slix toctxt) #((:ins toctxt) (str output-line "\n") (:atr toctxt)))
  (assoc toctxt
         :output (str (:output toctxt) output-line "\n")))
