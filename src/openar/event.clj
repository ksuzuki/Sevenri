;; %! Copyright (C) 2011 Kei Suzuki  All rights reserved. !%
;; 
;; This file is part of Openar, a Clojure environment ("This Software").
;; 
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License version 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns openar.event
  (:use [openar config core defs log jvm refs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Event Structure
;;;; {:target oplix :event event-id :source oplix :info value :response value}
;;;;
;;;; Event IDs
;;;; Event IDs are keywords derived from either
;;;;     openar.event/openar
;;;;     openar.event/oplixes
;;;;     openar.event/oplix
;;;;     openar.event/oplix-error
;;;;     openar.event/response
;;;;     openar.event/reason
;;;;     openar.event/info
;;;;
;;;; Event Dispatching
;;;; The core event dispatch function, dispatch-event, sends an event to a
;;;; target oplix, meaning, calls the event handler of the target oplix for
;;;; the event. The event handler may return a response which will be stored
;;;; in the event. The dispatcher returns a future event and referencing it
;;;; will block until the event handler completes.
;;;; 
;;;; Protocol
;;;; All event handlers take a single argument 'event' and may return a
;;;; response. The :target value in the event is the same as the oplix
;;;; argument value.
;;;;
;;;; When an oplix wants to handle an openar event, define a function
;;;; corresponding to the openar event.
;;;; e.g. define the openar-start function to handle the openar-start event.
;;;;
;;;; When an oplix wants to handle an event regarding all oplixes or another
;;;; oplix, define functions starting with 'oplixes-' or 'oplix-'.
;;;; e.g. define the oplixes-opening function to handle the oplixes-opening
;;;; event.
;;;;
;;;; When an oplix wants to handle an event for itself, define a function
;;;; of which name corresponds to an oplix event without 'oplix-'.
;;;; e.g. define the opening function to handle the opening event for the
;;;; oplix.
;;;;
;;;; When an oplix wants to return a response to an event, create a response
;;;; using create-event-response with a response value and optionally the
;;;; reason. Then return the response.
;;;;
;;;; Predefined Responses
;;;; There are some predefined keywords as reponses. The keywords are
;;;; derived from openar.event/response.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *openar-events* (make-hierarchy))
(def *event-response-wait-millis* 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-last-event
  []
  @*last-event*)

(defn get-last-error-event
  []
  @*last-error-event*)

(defn get-last-global-event
  []
  @*last-global-event*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-id
  [id set]
  (def *openar-events* (derive *openar-events* id set)))

(defn define-openar-event-id
  [id]
  (define-event-id id ::openar))

(defn is-openar-event?
  [id]
  (isa? *openar-events* id ::openar))

(define-openar-event-id ::openar-starting)
(define-openar-event-id ::openar-quitting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-oplixes-event-id
  [id]
  (define-event-id id ::oplixes))

(defn is-oplixes-event?
  [id]
  (isa? *openar-events* id ::oplixes))

(define-oplixes-event-id ::oplixes-opening)
(define-oplixes-event-id ::oplixes-opened)
(define-oplixes-event-id ::oplixes-closing)
(define-oplixes-event-id ::oplixes-closed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-oplix-event-id
  [id]
  (define-event-id id ::oplix))

(defn define-oplix-error-event-id
  [id]
  (define-event-id id ::oplix-error))

(defn is-oplix-event?
  [id]
  (or (isa? *openar-events* id ::oplix)
      (isa? *openar-events* id ::oplix-error)))

(defn is-oplix-error-event?
  [id]
  (isa? *openar-events* id ::oplix-error))

(define-oplix-event-id ::oplix-creating)
(define-oplix-event-id ::oplix-created)
(define-oplix-error-event-id ::oplix-error-create)

(define-oplix-event-id ::oplix-opening)
(define-oplix-event-id ::oplix-frame-creating)
(define-oplix-event-id ::oplix-frame-created)
(define-oplix-event-id ::oplix-frame-loading)
(define-oplix-event-id ::oplix-frame-loaded)
(define-oplix-event-id ::oplix-opened)
(define-oplix-event-id ::oplix-open-canceled)
(define-oplix-error-event-id ::oplix-error-open)

(define-oplix-event-id ::oplix-saving)
(define-oplix-event-id ::oplix-saved)
(define-oplix-event-id ::oplix-save-canceled)
(define-oplix-error-event-id ::oplix-error-save)

(define-oplix-event-id ::oplix-closing)
(define-oplix-event-id ::oplix-closed)
(define-oplix-event-id ::oplix-close-canceled)
(define-oplix-error-event-id ::oplix-error-close)

(define-oplix-event-id ::oplix-deleting)
(define-oplix-event-id ::oplix-deleted)
(define-oplix-error-event-id ::oplix-error-delete)

(define-oplix-event-id ::oplix-purging)
(define-oplix-event-id ::oplix-purged)
(define-oplix-error-event-id ::oplix-error-purge)

(define-oplix-event-id ::oplix-ok-to-quit?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-response
  [val]
  (def *openar-events* (derive *openar-events* val ::response)))

(defn is-event-response?
  [val]
  (isa? *openar-events* val ::response))

(define-event-response ::response-exception-occurred)

(define-event-response ::response-donot-open)
(define-event-response ::response-donot-load)
(define-event-response ::response-donot-save)
(define-event-response ::response-donot-close)

(define-event-response ::response-suppress-xml-encoder-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-reason
  [val]
  (def *openar-events* (derive *openar-events* val ::reason)))

(defn is-event-reason?
  [val]
  (isa? *openar-events* val ::reason))

(define-event-reason ::reason-exception-occurred)
(define-event-reason ::reason-name-exists)
(define-event-reason ::reason-reload-on-failed)
(define-event-reason ::reason-singleton-oplix)
(define-event-reason ::reason-load-frame-failed)
(define-event-reason ::reason-save-error-on-closing)
(define-event-reason ::reason-trash-files-failed)
(define-event-reason ::reason-oplix-running)
(define-event-reason ::reason-oplix-file-exists)
(define-event-reason ::reason-create-oplix-file-failed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-info
  [val]
  (def *openar-events* (derive *openar-events* val ::info)))

(defn is-event-info?
  [val]
  (isa? *openar-events* val ::info))

(define-event-info ::info-save-on-close)
(define-event-info ::info-close-on-delete)
(define-event-info ::info-close-on-close-oplixes)
(define-event-info ::info-close-on-quit-openar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-event
  ([target-oplix event-id]
     (create-event target-oplix event-id target-oplix nil))
  ([target-oplix event-id source-oplix]
     (create-event target-oplix event-id source-oplix nil))
  ([target-oplix event-id source-oplix info]
     (create-event target-oplix event-id source-oplix info :no-response))
  ([target-oplix event-id source-oplix info response]
     {:target target-oplix :event event-id :source source-oplix :info info :response response}))

(defn create-event-response
  [res-value & res-reason]
  {:value res-value :reason res-reason})

(defn set-event-response
  [event response]
  (let [event (if (future? event) @event event)]
    (assoc event :response response)))

(defmacro get-event-data
  [k e]
  `(~k (if (future? ~e) (deref ~e) ~e)))

(defmacro get-event-target   [event] `(get-event-data :target   ~event))
(defmacro get-event-id       [event] `(get-event-data :event    ~event))
(defmacro get-event-source   [event] `(get-event-data :source   ~event))
(defmacro get-event-info     [event] `(get-event-data :info     ~event))

(defn get-event-response
  [event]
  (let [res (get-event-data :response event)]
    (if (map? res)
      [(:value res) (:reason res)]
      [res nil])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro event-response-donot-save
  []
  `(create-event-response :openar.event/response-donot-save))

(defmacro event-response-donot-close
  []
  `(create-event-response :openar.event/response-donot-close))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is-event-reason-exception-occurred?
  [event]
  `(= (:reason (get-event-info ~event)) :openar.event/reason-exception-occurred))

(defmacro is-event-reason-reload-on-failed?
  [event]
  `(= (:reason (get-event-info ~event)) :openar.event/reason-reload-on-failed))

(defmacro is-event-reason-save-error-on-closing?
  [event]
  `(= (:reason (get-event-info ~event)) :openar.event/reason-save-error-on-closing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is-event-info-save-on-close?
  [event]
  `(true? (:openar.event/info-save-on-close (get-event-info ~event))))

(defmacro is-event-info-close-on-delete?
  [event]
  `(true? (:openar.event/info-close-on-delete (get-event-info ~event))))

(defmacro is-event-info-close-on-close-oplixes?
  [event]
  `(true? (:openar.event/info-close-on-close-oplixes (get-event-info ~event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These fns are resolved at the startup time.
(using-fns event oplix
           [oplix-on oplix-name get-oplix-fqns invoke-later])

(defn get-event-handler
  [event-id fqon]
  (ns-resolve fqon (symbol (name event-id))))

(defn get-oplixless-event-handler
  [event-id fqon]
  (when (is-oplix-event? event-id)
    (ns-resolve fqon (symbol (subs (name event-id) 6))))) ;; 6 := 'oplix-'

(defn- -dispatch-event
  "Return a future event or nil"
  ([tgt-oplix event-id]
     (-dispatch-event tgt-oplix event-id nil nil))
  ([tgt-oplix event-id src-oplix]
     (-dispatch-event tgt-oplix event-id src-oplix nil))
  ;;
  ([tgt-oplix event-id src-oplix info]
     (let [event (create-event tgt-oplix event-id src-oplix info)]
       ;; Remember the event.
       (reset! *last-event* event)
       (when (is-oplix-error-event? event-id)
         (reset! *last-error-event* event))
       ;; Dispatch the event.
       (let [fqon (event-using-get-oplix-fqns-oplix (event-using-oplix-on-oplix tgt-oplix))]
         (when-let [event-handler (if (and (is-oplix-event? event-id) (identical? tgt-oplix src-oplix))
                                    (get-oplixless-event-handler event-id fqon)
                                    (get-event-handler event-id fqon))]
           (let [bfn (bound-fn []
                               (let [post-event (atom event)
                                     event-lock (proxy [Object][])
                                     event-handler-invoker (fn []
                                                             (try
                                                               (reset! post-event
                                                                       (set-event-response event (event-handler event)))
                                                               (catch Exception e
                                                                 (log-exception e)
                                                                 (reset! post-event
                                                                         (set-event-response event ::response-exception-occurred)))
                                                               (finally
                                                                (unlock-and-resume event-lock))))]
                                 (lock-run-and-wait event-lock *event-response-wait-millis*
                                                    (event-using-invoke-later-oplix tgt-oplix event-handler-invoker))
                                 @post-event))]
             (future (bfn))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start post-event-to multimethods

(defmulti post-event-to
  "Return a future event or nil."
  (fn [tgt-oplix event-id & src-oplix-and-info]
    event-id))

(defmethod post-event-to :default
  [tgt-oplix event-id & src-oplix-and-info]
  (apply -dispatch-event tgt-oplix event-id src-oplix-and-info))

;;;; end post-event-to multimethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn send-event-to
  "Return an event, not a future event, when available. Otherwise, nil."
  [tgt-oplix event-id & src-oplix-and-info]
  (when-let [e (apply post-event-to tgt-oplix event-id src-oplix-and-info)]
    @e))

(defn post-event
  "Return a map of oplix name as key and a future event or nil as value."
  [event-id & src-oplix-and-info]
  (reduce (fn [m [name tgt-oplix]]
            (assoc m name (apply post-event-to tgt-oplix event-id src-oplix-and-info)))
          {}
          @*oplixes*))

(defn send-event
  "Return a map of oplix name as key and an event or nil as value."
  [event-id & src-oplix-and-info]
  (reduce (fn [m [name tgt-oplix]]
            (assoc m name (apply send-event-to tgt-oplix event-id src-oplix-and-info)))
          {}
          @*oplixes*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-and-send-global-event
  [eid]
  (reset! *last-global-event* eid)
  (send-event eid))

(defn set-and-post-global-event
  [eid]
  (reset! *last-global-event* eid)
  (post-event eid))

(defn send-openar-starting-event
  []
  (set-and-send-global-event :openar.event/openar-starting))

(defn send-oplixes-opening-event
  []
  (set-and-send-global-event :openar.event/oplixes-opening))

(defn post-oplixes-opened-event
  []
  (set-and-post-global-event :openar.event/oplixes-opened))

(defn send-oplixes-closing-event
  []
  (set-and-send-global-event :openar.event/oplixes-closing))

(defn send-oplixes-closed-event
  []
  (set-and-send-global-event :openar.event/oplixes-closed))

(defn send-openar-quitting-event
  []
  (set-and-send-global-event :openar.event/openar-quitting))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn post-creation-event
  "Special event post fn for the oplix being created. Needed because the
   oplix is not registered to *oplixes* yet."
  ([event-id source-oplix info]
     (post-creation-event event-id source-oplix info true))
  ([event-id source-oplix info post?]
     (if post?
       (let [fe (post-event-to source-oplix event-id source-oplix info)]
         (assoc (post-event event-id source-oplix info)
           (event-using-oplix-name-oplix source-oplix)
           fe))
       (let [e (send-event-to source-oplix event-id source-oplix info)]
         (assoc (send-event event-id source-oplix info)
           (event-using-oplix-name-oplix source-oplix)
           e)))))

(defn send-creation-event
  ([event-id source-oplix]
     (send-creation-event event-id source-oplix nil))
  ([event-id source-oplix info]
     (post-creation-event event-id source-oplix info false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn all-oplixes-ok-to-quit?
  "When an oplix doesnot want to quit, it should return false in response to
  the oplix-ok-to-quit? event."
  []
  (let [eid :openar.event/oplix-ok-to-quit?
        nes (reduce (fn [m [name tgt-oplix]]
                      (assoc m name (post-event-to tgt-oplix eid tgt-oplix)))
                    {}
                    @*oplixes*)]
    (reduce (fn [ok? [n e]]
              (let [[val reason] (get-event-response e)]
                (and ok? (if (not= val false)
                           true
                           false))))
            true
            nes)))
