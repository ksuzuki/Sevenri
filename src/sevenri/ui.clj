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

(ns sevenri.ui
  "Sevenri user interface lib"
  (:require [clojure.set :as cljset])
  (:use [sevenri config defs event log refs])
  (:import (clojure.lang IProxy)
           (java.awt AWTEvent Color Component EventQueue Font Toolkit)
           (java.awt.event AWTEventListener FocusEvent InvocationEvent
                           KeyAdapter KeyEvent WindowEvent)
           (java.beans EventHandler)
           (java.lang.reflect Proxy)
           (javax.swing BorderFactory JLabel PopupFactory)
           (javax.swing JInternalFrame JFrame)
           (java.util.logging Level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-awt-utilities-available?
  []
  *awt-utilities-available*)

;; These fns are resolved at the startup time.
(using-fns ui slix
           [is-slix? slix-frame
            put-slix-prop get-slix-prop remove-slix-prop
            get-system-event-queue get-slixes close-slix get-slix-sevenri])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn popup-log-notifier
  [level sec]
  (let [lvl (cond
             (instance? Level level) (.intValue level)
             (instance? Integer level) level
             :else (.intValue Level/OFF))]
    (when (< lvl (.intValue Level/OFF))
      (let [[lvs fgc bgc] (cond
                            (<= (.intValue Level/SEVERE) lvl) ["A SEVERE" Color/red Color/yellow]
                            (<= (.intValue Level/WARNING) lvl) ["A WARNING" Color/yellow Color/black]
                            (<= (.intValue Level/INFO) lvl) ["An INFO" Color/blue (Color. 255 255 204)]
                            :else ["A " Color/black Color/white])
            msg (str " " lvs " message has been logged ")
            lbl (JLabel. msg JLabel/CENTER)]
        (doto lbl
          (.setOpaque true)
          (.setFont (Font. "Helvetica" Font/PLAIN 14))
          (.setForeground fgc)
          (.setBackground bgc)
          (.setBorder (BorderFactory/createLineBorder bgc 4)))
        (let [sdm (.getScreenSize (Toolkit/getDefaultToolkit))
              dim (.getPreferredSize lbl)
              wdt (.getWidth dim)
              hgt (.getHeight dim)
              lcx (- (.getWidth sdm) wdt)
              lcy (- (.getHeight sdm) hgt)
              pup (.getPopup (PopupFactory/getSharedInstance) nil lbl lcx lcy)]
          (.postEvent (ui-using-get-system-event-queue-slix)
                      (InvocationEvent. (Object.)
                                        #(future
                                           (.show pup)
                                           (Thread/sleep (* 1000 sec))
                                           (.hide pup)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-z-order
  []
  @*z-order*)

(defn get-new-z-order-map
  [zomap front frame]
  (if (contains? zomap frame)
    ;; frame should be reachable from the front.
    ;; front -> ... -> frame -> ...
    (loop [f front]
      (if (identical? (get zomap f) frame)
        ;; remap to get: frame -> front -> ... -> f -> frame-next
        (let [frame-next (get zomap (get zomap f))]
          (assoc zomap
            frame front
            f frame-next))
        (if (get zomap f)
          (recur (get zomap f))
          zomap))) ;; in case frame wasn't found
    ;; a new frame. Remap to get: frame -> front -> ...
    (assoc zomap
      frame front)))
        
(defn update-z-order
  ([]
     (let [live-frames-set (apply hash-set (map #(ui-using-slix-frame-slix %) (ui-using-get-slixes-slix)))
           [zomap front] (get-z-order)]
       (reset! *z-order* (update-z-order zomap front live-frames-set))))
  ([frame]
     (let [[zomap front] (get-z-order)
           new-zomap (get-new-z-order-map zomap front frame)]
       (reset! *z-order* [new-zomap frame])))
  ;;
  ([zomap front live-frames-set]
     (if (and (seq zomap) front (contains? zomap front))
       (if (contains? live-frames-set front)
         (update-z-order zomap front live-frames-set front)
         (recur (dissoc zomap front) (get zomap front) live-frames-set))
       [{} nil]))
  ([zomap front live-frames-set new-front]
     (if-let [frame (get zomap front)]
       (if (contains? live-frames-set frame)
         (recur zomap frame live-frames-set new-front)
         (let [tzmap (assoc zomap front (get zomap frame))]
           (recur (dissoc tzmap frame) front live-frames-set new-front)))
       [zomap new-front])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-z-ordered-frames
  []
  (update-z-order)
  (let [[zomap front] (get-z-order)]
    (when front
      (loop [frames [front]
             frame front]
        (if-let [next-frame (get zomap frame)]
          (recur (conj frames next-frame) next-frame)
          frames)))))

(defn create-frame-popup
  [frame]
  (let [loc (.getLocationOnScreen frame)
        lbl (JLabel.)
        fgc Color/white
        bgc (Color. 102 0 102)
        pup (.getPopup (PopupFactory/getSharedInstance) nil lbl (.x loc) (.y loc))]
    (doto lbl
      (.setFont (Font. "Helvetica" Font/PLAIN 16))
      (.setText (str " " (.getTitle frame) " "))
      (.setOpaque true)
      (.setForeground fgc)
      (.setBackground bgc)
      (.setBorder (BorderFactory/createLineBorder bgc 2)))
    pup))

(defn move-frame-focus
  [event keycode forward? in?]
  (if in?
    (when-let [frames (or @*frames-snapshot* (get-z-ordered-frames))]
      (when (< 1 (count frames))
        (let [frames (if forward?
                       (concat (rest frames) (list (first frames)))
                       (concat (list (last frames)) (butlast frames)))
              frame (first frames)
              popup (when frame (create-frame-popup frame))]
          (.consume event)
          (when @*frame-popup*
            (.hide @*frame-popup*))
          (when popup
            (.show popup))
          (reset! *frame-popup* popup)
          (reset! *frames-snapshot* frames))))
    ;;
    (let [popup @*frame-popup*
          frame (first @*frames-snapshot*)]
      (when popup
        (.hide popup)
        (reset! *frame-popup* nil))
      (when frame
        (.consume event)
        (when (= keycode KeyEvent/VK_META)
          (.toFront frame))
        (reset! *frames-snapshot* nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Default key and window listeners

(defn default-close-window
  [frame]
  (let [fo (filter #(= frame (ui-using-slix-frame-slix %)) (ui-using-get-slixes-slix))]
    (if-let [slix (first fo)]
      (ui-using-close-slix-slix slix)
      (.dispose frame))))

(defn default-window-closing-listener
  [event]
  (let [frame (.getWindow event)]
    (default-close-window (if (instance? JFrame frame)
                            frame
                            (.getTopLevelAncestor frame)))))

(defn default-window-gained-focus-listener
  [event]
  (update-z-order (.getWindow event)))

(defn get-default-window-listeners
  []
  {:windowClosing default-window-closing-listener
   :windowGainedFocus default-window-gained-focus-listener})

(defn add-default-window-listener
  [frame]
  (let [dwlc (Class/forName (str (get-config 'src.sevenri.listeners.defwinlistener)))
        dwl (.newInstance dwlc)]
    (.addWindowListener frame dwl)
    (.addWindowFocusListener frame dwl)))

;;;;

(defn default-key-pressed-listener
  [event]
  (when (and (instance? KeyEvent event)
             (not (.isConsumed event)))
    (let [kc (.getKeyCode event)
          mk (.getModifiers event)]
      (cond
       ;; META+W: close window
       (and (= kc KeyEvent/VK_W) (pos? (bit-and mk KeyEvent/META_MASK)))
         (loop [comp (.getComponent event)]
           (when comp
             (if (or (instance? JFrame comp) (instance? JInternalFrame comp))
               (do
                 (.consume event)
                 (default-close-window comp))
               (recur (.getParent comp)))))))))

(defn get-default-key-listeners
  []
  {:keyPressed default-key-pressed-listener})

(defn add-default-key-listener
  [comp]
  (let [dklc (Class/forName (str (get-config 'src.sevenri.listeners.defkeylistener)))
        dkl (.newInstance dklc)]
    (.addKeyListener comp dkl)))

;;;;

(defmacro def-listener-method
  "def-x-listener support macro"
  [listeners method]
  (let [lsns# (symbol (str \. listeners))
        mthd# (symbol (str \- method))
        mkwd# (keyword (str method))]
    `(defn ~mthd#
       [~'this ~'event]
       (when-let [~'listener (~mkwd# @(~lsns# ~'this))]
         (~'listener ~'event)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro get-AWTUtilities-class
  []
  `(Class/forName "com.sun.awt.AWTUtilities"))

(defmacro invoke-awt-utils-static-method
  ([^String method]
     `(.invoke (.getDeclaredMethod (get-AWTUtilities-class) ~method nil)
               nil nil))
  ([^String method arg1 ca-arg1]
     `(.invoke (.getDeclaredMethod (get-AWTUtilities-class) ~method ~ca-arg1)
               nil (into-array Object (vector ~arg1))))
  ([^String method arg1 arg2 ca-arg12]
     `(.invoke (.getDeclaredMethod (get-AWTUtilities-class) ~method ~ca-arg12)
               nil (into-array Object (vector ~arg1 ~arg2)))))

(defmacro is-awt-utils-feature-supported?
  ([^String feature]
     `(if (is-awt-utilities-available?)
        (invoke-awt-utils-static-method ~feature)
        false))
  ([^String feature arg1 ca-arg1]
     `(if (is-awt-utilities-available?)
        (invoke-awt-utils-static-method ~feature ~arg1 ~ca-arg1)
        false))
  ([^String feature arg1 arg2 ca-arg12]
     `(if (is-awt-utilities-available?)
        (invoke-awt-utils-static-method ~feature ~arg1 ~arg2 ~ca-arg12)
        false)))

(defn is-translucency-supported?
  [translucency]
  (is-awt-utils-feature-supported? "isTranslucencySupported"
                                   translucency (into-array Class [(class translucency)])))

(defmacro get-AWTUtilities-Translucency
  []
  `(Class/forName "com.sun.awt.AWTUtilities$Translucency"))

(defn get-awt-utils-translucencies
  []
  (let [vs (.getEnumConstants (get-AWTUtilities-Translucency))
        ks (map #(keyword (str %)) vs)]
    (apply hash-map (interleave ks vs))))

(defmacro invoke-awt-utils-feature
  ([^String feature arg1 ca-arg1]
     `(if (is-awt-utilities-available?)
        (invoke-awt-utils-static-method ~feature ~arg1 ~ca-arg1)))
  ([^String feature arg1 arg2 ca-arg2]
     `(if (is-awt-utilities-available?)
        (invoke-awt-utils-static-method ~feature ~arg1 ~arg2 ~ca-arg2))))

;;;;

(defn is-translucency-capable?
  [gconfig]
  (is-awt-utils-feature-supported? "isTranslucencyCapable"
                                   gconfig (into-array Class [(class gconfig)])))

(defn is-perpixel-transparent-supported?
  []
  (is-translucency-supported? (:PERPIXEL_TRANSPARENT (get-awt-utils-translucencies))))

(defn is-translucent-supported?
  []
  (is-translucency-supported? (:TRANSLUCENT (get-awt-utils-translucencies))))

(defn is-perpixel-translucent-supported?
  []
  (is-translucency-supported? (:PERPIXEL_TRANSLUCENT (get-awt-utils-translucencies))))

(defn get-window-opacity
  [window]
  (invoke-awt-utils-feature "getWindowOpacity"
                            window (into-array Class [java.awt.Window])))

(defn set-window-opacity
  [window fval]
  (invoke-awt-utils-feature "setWindowOpacity"
                            window fval (into-array Class [java.awt.Window Float/TYPE])))

(defn set-window-opaque
  [window bval]
  (invoke-awt-utils-feature "setWindowOpaque"
                            window bval (into-array Class [java.awt.Window Boolean/TYPE])))

(defn get-window-shape
  [window]
  (invoke-awt-utils-feature "getWindowShape"
                            window (into-array Class [java.awt.Window])))

(defn set-window-shape
  [window shape]
  (invoke-awt-utils-feature "setWindowShape"
                            window shape (into-array Class [java.awt.Window java.awt.Shape])))

(defn set-component-mixing-cutout-shape
  [component shape]
  (invoke-awt-utils-feature "setComponentMixingCutoutShape"
                            component shape (into-array Class [java.awt.Component java.awt.Shape])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -get-event-delegator-class
  []
  *event-delegator-class*)

(defn- -get-event-delegator-persistence-delegate
  []
  (.invoke (.getMethod (-get-event-delegator-class) "getPersistenceDelegate" nil) nil nil))

(defn- -get-event-delegator
  ([]
     (-get-event-delegator (gensym "ed")))
  ([id]
     (let [ctor (.getConstructor (-get-event-delegator-class) (into-array Class [String]))]
       (.newInstance ctor (into-array [(str id)])))))

(defn- -set-event-handler-to-delegator
  ([handler]
     (-set-event-handler-to-delegator handler (-get-event-delegator)))
  ([handler event-delegator]
     (if (and handler (instance? (-get-event-delegator-class) event-delegator))
       (do
         (.setHandler event-delegator handler)
         event-delegator)
       (throw (IllegalArgumentException.
               "-set-event-handler-to-delegator: null-handler or invalid event-delegator")))))

;;;;

(defn set-event-delegator-persistence-delegate
  [xml-encoder]
  (let [ed (-get-event-delegator-class)
        pd (-get-event-delegator-persistence-delegate)]
    (.setPersistenceDelegate xml-encoder ed pd)))

(defn set-event-handlers
  "Set an event handler for an event listener method to an event delegator.
   listener-intf is a listener interface and id-method-handler-map is a map
   of handling id as key and a pair of listener method and handler in vector
   as value. Each pair of listener method and handler is bound to the
   handling id (a symbol or a string), and the event delegator with the same
   id is assigned the handler for the listener method.
   When no event delegator for and id is found, a new event delegator with
   the id is created and assigned the corresponding handler for method.
   If remove-unref-delegators? is true, the delegators of which ids not
   specified in id-method-handler-map are removed.

   id-method-handler-map := { id1 [listener-method-a handler-a],
                              id2 [listener-method-b handler-b],
                               :           :              :
                              idn [listener-method-x handler-x] }

   listener-intf is such like 'java.awt.event.ActionListener'. listener-method
   is a symbol or a string, like 'actionPerformed. it can be nil, and in
   that case the corresponding handler receives the event object for all
   listener methods defined by listener-intf."
  ([comp listener-intf id-method-handler-map]
     (set-event-handlers comp listener-intf id-method-handler-map false))
  ([comp listener-intf id-method-handler-map remove-unref-delegators?]
     (let [comp-class (.getClass comp)
           intf-name (last (.split (str (.getName listener-intf)) "\\."))
           listener-intf-array (into-array [listener-intf])
           ;;
           add-listener-name (str "add" intf-name)
           add-listener-method (.getMethod comp-class add-listener-name listener-intf-array)
           get-listeners-name (str "get" intf-name "s")
           get-listeners-method (.getMethod comp-class get-listeners-name nil)
           remove-listener-name (str "remove" intf-name)
           remove-listener-method (.getMethod comp-class remove-listener-name listener-intf-array)
           ;;
           get-handler-target (fn [l] (.getTarget (Proxy/getInvocationHandler l)))
           event-delegator-listeners (filter #(and (instance? Proxy %)
                                                   (Proxy/isProxyClass (.getClass %))
                                                   (instance? (-get-event-delegator-class) (get-handler-target %)))
                                             (.invoke get-listeners-method comp (into-array [])))
           id-delegator-map (reduce (fn [m l]
                                      (let [ed (get-handler-target l)]
                                        (assoc m (.getId ed) ed)))
                                    {}
                                    event-delegator-listeners)
           method-handler-ids (apply hash-set (map str (keys id-method-handler-map)))
           delegator-ids (apply hash-set (keys id-delegator-map))
           unset-method-handler-ids (cljset/difference method-handler-ids delegator-ids)
           unref-delegator-ids (cljset/difference delegator-ids method-handler-ids)]
       #_(lg "nid-delegator-map:" id-delegator-map "\n"
           "unset-method-handler-ids:" unset-method-handler-ids "\n"
           "unref-delegator-ids:" unref-delegator-ids)
       ;; Assign handler to delegator
       (doseq [[id delegator] id-delegator-map]
         (when-let [method-handler (or (get id-method-handler-map (symbol id)) (get id-method-handler-map id))]
           (let [[method handler] method-handler]
             (-set-event-handler-to-delegator handler delegator))))
       ;; Add new event delegator for unset handler/method.
       (when (seq unset-method-handler-ids)
         (doseq [id unset-method-handler-ids]
           (let [[method handler] (or (get id-method-handler-map (symbol id)) (get id-method-handler-map id))
                 delegator (-set-event-handler-to-delegator handler (-get-event-delegator id))
                 ;; Specify "" for the 4th arg to get whole event object in the handler.
                 event-handler (EventHandler/create listener-intf delegator "handleEvent" "" (when method (str method)))
                 delegator-listener event-handler]
             ;; Clojure's .invoke is expecting args in array...
             (.invoke add-listener-method comp (into-array [delegator-listener])))))
       ;; Remove unreferened delegators if requested.
       (when (and remove-unref-delegators? (seq unref-delegator-ids))
         (doseq [id unref-delegator-ids]
           (doseq [event-delegator-listener event-delegator-listeners]
             (when (= id (.getId (get-handler-target event-delegator-listener)))
               (.invoke remove-listener-method comp (into-array [event-delegator-listener])))))))))

(defn set-event-handler-set
  "Given a seq of the pairs of listener-intf and id-method-handler-map, call
   set-event-handlers for each pair with comp."
  [comp & listener-intf-id-method-handler-maps]
  (when-not (even? (count listener-intf-id-method-handler-maps))
    (throw (IllegalArgumentException. "set-event-handler-set: odd listener-intf/id-method-handler-map")))
  (loop [pairs listener-intf-id-method-handler-maps]
    (when (seq pairs)
      (set-event-handlers comp (first pairs) (second pairs))
      (recur (nnext pairs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated - remove by 0.3.0

;;;; dynaclass := clojure.lang.IProxy | AnonymousClass

(defmacro create-listener-access-triplet-vector
  [base]
  (let [adr# (symbol (format ".add%sListener" base))
        gtr# (symbol (format ".get%sListeners" base))
        rmr# (symbol (format ".remove%sListener" base))]
    `[(fn [~'c ~'o] (~adr# ~'c ~'o))
      (fn [~'c] (~gtr# ~'c))
      (fn [~'c ~'o] (~rmr# ~'c ~'o))]))

(defn clear-saved-dynaclass-listeners
  [slix]
  (ui-using-remove-slix-prop-slix slix *saved-dynaclass-listeners*))

(defn restore-saved-dynaclass-listeners
  "[ [comp [[adder dc-listeners]*]* ]"
  [slix]
  (when-let [calv-vec (ui-using-get-slix-prop-slix slix *saved-dynaclass-listeners*)]
    (when (seq calv-vec)
      (doseq [[comp alv-vec] calv-vec]
        (when (instance? Component comp)
          (doseq [[adr dls] alv-vec]
            (doseq [dl dls]
              (when (and (fn? adr) (or (instance? IProxy dl)
                                       (.isAnonymousClass (class dl))))
                (adr comp dl))))))
      (clear-saved-dynaclass-listeners slix))))

(defn remove-dynaclass-listeners
  "In:  comp [[adder getter remover]*]
   Out: [comp [[adder dc-objects]*]] or nil."
  [comp agrv-vec]
  (when (and (instance? Component comp) (seq agrv-vec))
    (let [alv-vec (reduce (fn [alvv [adr gtr rmr]]
                            (if (and (fn? adr) (fn? gtr) (fn? rmr))
                              (let [ls (gtr comp)]
                                (if (seq ls)
                                  (loop [i (dec (alength ls))
                                         dls nil]
                                    (if (<= 0 i)
                                      (let [l (aget ls i)]
                                        (if (or (instance? IProxy l)
                                                (.isAnonymousClass (class l)))
                                          (do
                                            (rmr comp l)
                                            (recur (dec i) (cons l dls)))
                                          (recur (dec i) dls)))
                                      (if (seq dls)
                                        (conj alvv [adr dls])
                                        alvv)))
                                  alvv))
                              alvv))
                          []
                          agrv-vec)]
      (when (seq alv-vec)
        [comp alv-vec]))))

(defn save-dynaclass-listeners
  "In:  [ [comp [[adder getter remover]*]]* ]
   Out: [ [comp [[adder dc-listeners]*]]* ]"
  [slix cagrv-vec]
  (when (ui-using-is-slix?-slix slix)
    (let [calv-vec (reduce (fn [calvv [comp agrvv]]
                             (if (and comp agrvv)
                               (if-let [calv (remove-dynaclass-listeners comp agrvv)]
                                 (conj calvv calv)
                                 calvv)
                               calvv))
                           []
                           cagrv-vec)]
      (when (seq calv-vec)
        (ui-using-put-slix-prop-slix slix *saved-dynaclass-listeners* calv-vec)))))

;;; shorter, macro versions

(defmacro listener-triplet
  [listener]
  `(create-listener-access-triplet-vector ~listener))

(defmacro clear-dyna-listeners
  ([]
     `(restore-saved-dynaclass-listeners ~'*slix*))
  ([slix]
     `(restore-saved-dynaclass-listeners ~slix)))

(defmacro remove-dyna-listeners
  [comp agrv-vec]
  `(remove-dynaclass-listeners ~comp ~agrv-vec))

(defmacro save-dyna-listeners
  ([cagrv-vec]
     `(save-dynaclass-listeners ~'*slix* ~cagrv-vec))
  ([slix cagrv-vec]
     `(save-dynaclass-listeners ~slix ~cagrv-vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; startup/shutdown

(defn- -add-awt-event-listeners?
  []
  (let [tk (Toolkit/getDefaultToolkit)]
    ;; KeyEvent: move frame focus with META+` key
    (.addAWTEventListener
     tk
     (proxy [AWTEventListener] []
       (eventDispatched [event]
         (let [id (.getID event)
               kc (.getKeyCode event)
               mk (.getModifiers event)]
           (cond
            (= id KeyEvent/KEY_PRESSED)
              (do
                ;; META+`: get in or continue the move frame focus state
                (when (and (= kc KeyEvent/VK_BACK_QUOTE) (pos? (bit-and mk KeyEvent/META_MASK)))
                  (move-frame-focus event kc (if (pos? (bit-and mk KeyEvent/SHIFT_MASK)) false true) true)))
            (= id KeyEvent/KEY_RELEASED)
              (do
                ;; META or ESCAPE: get out of the move frame focus state
                (when (or (= kc KeyEvent/VK_META) (= kc KeyEvent/VK_ESCAPE))
                  (move-frame-focus event kc true false)))))))
     AWTEvent/KEY_EVENT_MASK)
    ;; WindowEvent: make sure at least one frame is opened
    (.addAWTEventListener
     tk
     (proxy [AWTEventListener] []
       (eventDispatched [event]
         (when (and (instance? JFrame (.getWindow event))
                    (= WindowEvent/WINDOW_CLOSED (.getID event))
                    (not= (get-last-global-event) :sevenri.event/sevenri-quitting))
           (let [ops (ui-using-get-slixes-slix)]
             (cond
              (= (count ops) 1)
              (let [frame (ui-using-slix-frame-slix (first ops))]
                (EventQueue/invokeLater #(.toFront frame)))
              (every? true? (map #(pos? (bit-and (.getExtendedState (ui-using-slix-frame-slix %)) JFrame/ICONIFIED)) ops))
              (let [sevenri (ui-using-get-slix-sevenri-slix)]
                    (.toFront (ui-using-slix-frame-slix (or sevenri (first ops))))))))))
     AWTEvent/WINDOW_EVENT_MASK)
    ;; FocusEvent: for focus related debug
    #_(.addAWTEventListener
     tk
     (proxy [AWTEventListener] []
       (eventDispatched [event]
         (let [id (.getID event)
               cmp (.getComponent event)
               op-cmp (.getOppositeComponent event)]
           (cond
            (= id FocusEvent/FOCUS_GAINED) (lg "fg cmp:" cmp "op-cmp:" op-cmp)
            (= id FocusEvent/FOCUS_LOST) (lg "fl cmp:" cmp "op-cmp:" op-cmp)
            :else (lg "fe id:" id "cmp:" cmp "op-cmp:" op-cmp)))))
     AWTEvent/FOCUS_EVENT_MASK))
  ;;
  true)

(defn- -setup-event-delegator-class?
  []
  (let [evtdelegator-class (Class/forName (str (get-config 'src.sevenri.listeners.evtdelegator)))]
    (redef! *event-delegator-class* evtdelegator-class)
    true))

;;;;

(defn startup-ui?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -add-awt-event-listeners?
          -setup-event-delegator-class?)))

(defn shutdown-ui?
  []
  (apply while-each-true?
         nil))
