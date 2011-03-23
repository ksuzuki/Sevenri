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
  [pline warn-attr]
  {:print-line pline :attr nil :wattr warn-attr :output ""})

(defmulti handle-test-output
  (fn [output-line toctxt]
    (cond
     (re-matches re-xml-header output-line) :xml-header
     (re-matches re-beg-xml-element output-line) :beg-xml-element
     (re-matches re-end-xml-element output-line) :end-xml-element
     :else :default)))

(defmethod handle-test-output :default
  [output-line toctxt]
  ((:print-line toctxt) (str output-line "\n") (:attr toctxt))
  (assoc toctxt
         :output (str (:output toctxt) output-line "\n")))

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
          msg (format "\nTesting %s.%s\n" pkg nme)]
      (when-not (empty? pkg)
        ((:print-line toctxt) msg))
      (assoc toctxt
        :output (str (:output toctxt) xml-element "\n")))
    ;;
    (let [s (second (re-matches re-beg-xml-element xml-element))
          a (when (or (re-matches re-beg-failure xml-element)
                      (re-matches re-beg-error xml-element))
              (:wattr toctxt))]
      (when s
        ((:print-line toctxt) (str s "\n") a))
      (assoc toctxt
        :attr a
        :output (str (:output toctxt) xml-element "\n")))))

(defmethod handle-test-output :end-xml-element
  [xml-element toctxt]
  (let [s (second (re-matches re-end-xml-element xml-element))]
    (when s
      ((:print-line toctxt) (str s "\n") (:attr toctxt)))
    (assoc toctxt
      :attr nil
      :output (str (:output toctxt) xml-element "\n"))))
