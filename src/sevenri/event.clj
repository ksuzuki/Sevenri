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

(ns sevenri.event
  (:use [sevenri config core defs log jvm refs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Event Structure
;;;; {:target slix :event event-id :source slix :info value :response value}
;;;;
;;;; Event IDs
;;;; Event IDs are keywords derived from either
;;;;     sevenri.event/sevenri
;;;;     sevenri.event/slixes
;;;;     sevenri.event/slix
;;;;     sevenri.event/slix-error
;;;;     sevenri.event/response
;;;;     sevenri.event/reason
;;;;     sevenri.event/info
;;;;
;;;; Event Dispatching
;;;; The core event dispatch function, dispatch-event, sends an event to a
;;;; target slix, meaning, calls the event handler of the target slix for
;;;; the event. The event handler may return a response which will be stored
;;;; in the event. The dispatcher returns a future event and referencing it
;;;; will block until the event handler completes.
;;;; 
;;;; Protocol
;;;; All event handlers take a single argument 'event' and may return a
;;;; response. The :target value in the event is the same as the slix
;;;; argument value.
;;;;
;;;; When a slix wants to handle an sevenri event, define a function
;;;; corresponding to the sevenri event.
;;;; e.g. define the sevenri-start function to handle the sevenri-start event.
;;;;
;;;; When a slix wants to handle an event regarding all slixes or another
;;;; slix, define functions starting with 'slixes-' or 'slix-'.
;;;; e.g. define the slixes-opening function to handle the slixes-opening
;;;; event.
;;;;
;;;; When a slix wants to handle an event for itself, define a function
;;;; of which name corresponds to a slix event without 'slix-'.
;;;; e.g. define the opening function to handle the opening event for the
;;;; slix.
;;;;
;;;; When a slix wants to return a response to an event, create a response
;;;; using create-event-response with a response value and optionally the
;;;; reason. Then return the response.
;;;;
;;;; Predefined Responses
;;;; There are some predefined keywords as reponses. The keywords are
;;;; derived from sevenri.event/response.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *sevenri-events* (make-hierarchy))
(def *event-response-wait-millis* 1000)
(def *slix-dash-len* (inc (count (str (get-default :src :slix :dir-name)))))

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
  (def *sevenri-events* (derive *sevenri-events* id set)))

(defn define-sevenri-event-id
  [id]
  (define-event-id id ::sevenri))

(defn is-sevenri-event?
  [id]
  (isa? *sevenri-events* id ::sevenri))

(define-sevenri-event-id ::sevenri-starting)
(define-sevenri-event-id ::sevenri-quitting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-slixes-event-id
  [id]
  (define-event-id id ::slixes))

(defn is-slixes-event?
  [id]
  (isa? *sevenri-events* id ::slixes))

(define-slixes-event-id ::slixes-opening)
(define-slixes-event-id ::slixes-opened)
(define-slixes-event-id ::slixes-closing)
(define-slixes-event-id ::slixes-closed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-slix-event-id
  [id]
  (define-event-id id ::slix))

(defn define-slix-error-event-id
  [id]
  (define-event-id id ::slix-error))

(defn is-slix-event?
  [id]
  (or (isa? *sevenri-events* id ::slix)
      (isa? *sevenri-events* id ::slix-error)))

(defn is-slix-error-event?
  [id]
  (isa? *sevenri-events* id ::slix-error))

(define-slix-event-id ::slix-creating)
(define-slix-event-id ::slix-created)
(define-slix-error-event-id ::slix-error-create)

(define-slix-event-id ::slix-opening)
(define-slix-event-id ::slix-frame-creating)
(define-slix-event-id ::slix-frame-created)
(define-slix-event-id ::slix-frame-loading)
(define-slix-event-id ::slix-frame-loaded)
(define-slix-event-id ::slix-opened)
(define-slix-event-id ::slix-open-canceled)
(define-slix-event-id ::slix-open-after-building-project)
(define-slix-error-event-id ::slix-error-open)

(define-slix-event-id ::slix-saving)
(define-slix-event-id ::slix-saved)
(define-slix-event-id ::slix-save-canceled)
(define-slix-error-event-id ::slix-error-save)

(define-slix-event-id ::slix-closing)
(define-slix-event-id ::slix-closed)
(define-slix-event-id ::slix-close-canceled)
(define-slix-error-event-id ::slix-error-close)

(define-slix-event-id ::slix-deleting)
(define-slix-event-id ::slix-deleted)
(define-slix-error-event-id ::slix-error-delete)

(define-slix-event-id ::slix-purging)
(define-slix-event-id ::slix-purged)
(define-slix-error-event-id ::slix-error-purge)

(define-slix-event-id ::slix-ok-to-quit?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-response
  [val]
  (def *sevenri-events* (derive *sevenri-events* val ::response)))

(defn is-event-response?
  [val]
  (isa? *sevenri-events* val ::response))

(define-event-response ::response-exception-occurred)

(define-event-response ::response-donot-open)
(define-event-response ::response-donot-load)
(define-event-response ::response-donot-save)
(define-event-response ::response-donot-close)

(define-event-response ::response-suppress-xml-encoder-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-reason
  [val]
  (def *sevenri-events* (derive *sevenri-events* val ::reason)))

(defn is-event-reason?
  [val]
  (isa? *sevenri-events* val ::reason))

(define-event-reason ::reason-exception-occurred)
(define-event-reason ::reason-name-exists)
(define-event-reason ::reason-reload-sn-failed)
(define-event-reason ::reason-singleton-slix)
(define-event-reason ::reason-load-frame-failed)
(define-event-reason ::reason-save-error-on-closing)
(define-event-reason ::reason-trash-files-failed)
(define-event-reason ::reason-slix-running)
(define-event-reason ::reason-slix-file-exists)
(define-event-reason ::reason-create-slix-file-failed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn define-event-info
  [val]
  (def *sevenri-events* (derive *sevenri-events* val ::info)))

(defn is-event-info?
  [val]
  (isa? *sevenri-events* val ::info))

(define-event-info ::info-save-on-close)
(define-event-info ::info-close-on-delete)
(define-event-info ::info-close-on-close-slixes)
(define-event-info ::info-close-on-quit-sevenri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-event
  ([target-slix event-id]
     (create-event target-slix event-id target-slix nil))
  ([target-slix event-id source-slix]
     (create-event target-slix event-id source-slix nil))
  ([target-slix event-id source-slix info]
     (create-event target-slix event-id source-slix info :no-response))
  ([target-slix event-id source-slix info response]
     {:target target-slix :event event-id :source source-slix :info info :response response}))

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

(defmacro event-response-donot-open
  []
  `(create-event-response :sevenri.event/response-donot-open))

(defmacro event-response-donot-load
  []
  `(create-event-response :sevenri.event/response-donot-load))

(defmacro event-response-donot-close
  []
  `(create-event-response :sevenri.event/response-donot-close))

(defmacro event-response-donot-save
  []
  `(create-event-response :sevenri.event/response-donot-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is-event-reason-exception-occurred?
  [event]
  `(= (:reason (get-event-info ~event)) :sevenri.event/reason-exception-occurred))

(defmacro is-event-reason-reload-sn-failed?
  [event]
  `(= (:reason (get-event-info ~event)) :sevenri.event/reason-reload-sn-failed))

(defmacro is-event-reason-save-error-on-closing?
  [event]
  `(= (:reason (get-event-info ~event)) :sevenri.event/reason-save-error-on-closing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro is-event-info-save-on-close?
  [event]
  `(true? (:sevenri.event/info-save-on-close (get-event-info ~event))))

(defmacro is-event-info-close-on-delete?
  [event]
  `(true? (:sevenri.event/info-close-on-delete (get-event-info ~event))))

(defmacro is-event-info-close-on-close-slixes?
  [event]
  `(true? (:sevenri.event/info-close-on-close-slixes (get-event-info ~event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These fns are resolved at the startup time.
(using-fns event slix
           [slix-sn slix-name get-slix-fqns invoke-later])

(defn get-event-handler
  [event-id fqns]
  (ns-resolve fqns (symbol (name event-id))))

(defn get-slixless-event-handler
  [event-id fqns]
  (when (is-slix-event? event-id)
    (ns-resolve fqns (symbol (subs (name event-id) *slix-dash-len*)))))

(defn- -dispatch-event
  "Return a future event or nil"
  ([tgt-slix event-id]
     (-dispatch-event tgt-slix event-id nil nil))
  ([tgt-slix event-id src-slix]
     (-dispatch-event tgt-slix event-id src-slix nil))
  ;;
  ([tgt-slix event-id src-slix info]
     (let [event (create-event tgt-slix event-id src-slix info)]
       ;; Remember the event.
       (reset! *last-event* event)
       (when (is-slix-error-event? event-id)
         (reset! *last-error-event* event))
       ;; Dispatch the event.
       (let [fqns (event-using-get-slix-fqns-slix (event-using-slix-sn-slix tgt-slix))]
         (when-let [event-handler (if (and (is-slix-event? event-id) (identical? tgt-slix src-slix))
                                    (get-slixless-event-handler event-id fqns)
                                    (get-event-handler event-id fqns))]
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
                                                    (event-using-invoke-later-slix tgt-slix event-handler-invoker))
                                 @post-event))]
             (future (bfn))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start post-event-to multimethods

(defmulti post-event-to
  "Return a future event or nil."
  (fn [tgt-slix event-id & src-slix-and-info]
    event-id))

(defmethod post-event-to :default
  [tgt-slix event-id & src-slix-and-info]
  (apply -dispatch-event tgt-slix event-id src-slix-and-info))

;;;; end post-event-to multimethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn send-event-to
  "Return an event, not a future event, when available. Otherwise, nil."
  [tgt-slix event-id & src-slix-and-info]
  (when-let [e (apply post-event-to tgt-slix event-id src-slix-and-info)]
    @e))

(defn post-event
  "Return a map of slix name as key and a future event or nil as value."
  [event-id & src-slix-and-info]
  (reduce (fn [m [name tgt-slix]]
            (assoc m name (apply post-event-to tgt-slix event-id src-slix-and-info)))
          {}
          @*slixes*))

(defn send-event
  "Return a map of slix name as key and an event or nil as value."
  [event-id & src-slix-and-info]
  (reduce (fn [m [name tgt-slix]]
            (assoc m name (apply send-event-to tgt-slix event-id src-slix-and-info)))
          {}
          @*slixes*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-and-send-global-event
  [eid]
  (reset! *last-global-event* eid)
  (send-event eid))

(defn set-and-post-global-event
  [eid]
  (reset! *last-global-event* eid)
  (post-event eid))

(defn send-sevenri-starting-event
  []
  (set-and-send-global-event :sevenri.event/sevenri-starting))

(defn send-slixes-opening-event
  []
  (set-and-send-global-event :sevenri.event/slixes-opening))

(defn post-slixes-opened-event
  []
  (set-and-post-global-event :sevenri.event/slixes-opened))

(defn send-slixes-closing-event
  []
  (set-and-send-global-event :sevenri.event/slixes-closing))

(defn send-slixes-closed-event
  []
  (set-and-send-global-event :sevenri.event/slixes-closed))

(defn send-sevenri-quitting-event
  []
  (set-and-send-global-event :sevenri.event/sevenri-quitting))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn post-creation-event
  "Special event post fn for the slix being created. Needed because the
   slix is not registered to *slixes* yet."
  ([event-id source-slix info]
     (post-creation-event event-id source-slix info true))
  ([event-id source-slix info post?]
     (if post?
       (let [fe (post-event-to source-slix event-id source-slix info)]
         (assoc (post-event event-id source-slix info)
           (event-using-slix-name-slix source-slix)
           fe))
       (let [e (send-event-to source-slix event-id source-slix info)]
         (assoc (send-event event-id source-slix info)
           (event-using-slix-name-slix source-slix)
           e)))))

(defn send-creation-event
  ([event-id source-slix]
     (send-creation-event event-id source-slix nil))
  ([event-id source-slix info]
     (post-creation-event event-id source-slix info false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn all-slixes-ok-to-quit?
  "When a slix doesnot want to quit, it should return false in response to
  the slix-ok-to-quit? event."
  []
  (let [eid :sevenri.event/slix-ok-to-quit?
        nes (reduce (fn [m [name tgt-slix]]
                      (assoc m name (post-event-to tgt-slix eid tgt-slix)))
                    {}
                    @*slixes*)]
    (reduce (fn [ok? [n e]]
              (let [[val reason] (get-event-response e)]
                (and ok? (if (not= val false)
                           true
                           false))))
            true
            nes)))
