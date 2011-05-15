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
  (:use [sevenri config defs event java log props refs])
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
  (let [dwlc (Class/forName (str (get-config 'src.sevenri.listeners.default-window-listener)))
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
  (let [dklc (Class/forName (str (get-config 'src.sevenri.listeners.default-key-listener)))
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
;;
;; EventListener
;; Listener-X      EventHandler               
;;   method-x <--- method-x-handler(Proxy)    event-handler-target
;;                   "handleEvent" action     id
;;                     target <-------------- target <---------------------------+
;;                                            (handleEvent [e]                   |
;;                                              (when target (target e)) ===> handler-x

(defn- -get-event-handler-target-class
  []
  *event-handler-target-class*)

(defn- -get-event-handler-target-persistence-delegate
  []
  (.invoke (.getMethod (-get-event-handler-target-class) "getPersistenceDelegate" nil) nil nil))

(defn- -create-event-handler-target
  ([]
     (-create-event-handler-target (gensym "lms")))
  ([id]
     (let [ctor (.getConstructor (-get-event-handler-target-class) (into-array Class [String]))]
       (.newInstance ctor (into-array [(str id)])))))

(defn- -set-handler-to-target
  ([handler]
     (-set-handler-to-target handler (-create-event-handler-target)))
  ([handler target]
     (if (and handler (instance? (-get-event-handler-target-class) target))
       (do
         (.setHandler target handler)
         target)
       (throw (IllegalArgumentException.
               "-set-handler-to-target: null handler or invalid event-handler-target")))))

(defn- -get-listener-accessor-methods
  [comp-class listener-intf]
  (let [bean-info (java.beans.Introspector/getBeanInfo comp-class)
        event-set-descs (filter #(= listener-intf (.getListenerType %))
                                (.getEventSetDescriptors bean-info))]
    (if (seq event-set-descs)
      (let [esd (first event-set-descs)
            add-method (.getAddListenerMethod esd)
            get-method (.getGetListenerMethod esd)
            remove-method (.getRemoveListenerMethod esd)]
        (if (and add-method get-method remove-method)
          [add-method get-method remove-method]
          (throw (IllegalArgumentException. (str "-get-listener-method-triplet: " comp-class
                                                 " missing listener accessor method(s) for " listener-intf)))))
      (throw (IllegalArgumentException. (str "-get-listener-method-triplet: " comp-class
                                             " has no EventSetDescription for " listener-intf))))))

;;;;

(defn add-event-handler-target-persistence-delegate
  [xml-encoder]
  (let [ehtc (-get-event-handler-target-class)
        ehtpd (-get-event-handler-target-persistence-delegate)]
    (.setPersistenceDelegate xml-encoder ehtc ehtpd)))

(defn set-listener-handlers
  "For a listener interface, set up each listener method handler to handle
   the event called on the listener method of the listener interface.
   listener-intf is a listener interface name symbol, like
   'java.awt.event.ActionListener, and id-method-handler-map is a map of
   method id key and vector value consists of a pair of a listener method
   name and a handler.
   A listener method handler is associated to a listener method via a
   'target' object, which the listener method invokes with an event object
   to handle the event.
   When the listener method target for an id doesn't exist, a new one with
   the id is created and the handler is assigned to it.
   When remove-unused-targets? is true, listener method targets with no
   handler is assigned are removed.

   id-method-handler-map := { method-id1 [listener-method-a handler-a],
                              method-id2 [listener-method-b handler-b],
                               :           :              :
                              method-idn [listener-method-x handler-x] }

   method-id is a symbol or string. Likewise, listener-method is a symbol or
   a string, such as 'actionPerformed. It can be nil, and in that case the
   handler receives the event object for all listener methods defined by
   listener-intf."
  ([comp listener-intf id-method-handler-map]
     (set-listener-handlers comp listener-intf id-method-handler-map false))
  ([comp listener-intf id-method-handler-map remove-unused-targets?]
     (let [comp-class (.getClass comp)
           intf-name (last (.split (str (.getName listener-intf)) "\\."))
           listener-intf-array (into-array [listener-intf])
           ;;
           [add-listener-method get-listeners-method remove-listener-method]
             (-get-listener-accessor-methods comp-class listener-intf)
           ;;
           get-invocation-target (fn [l] (.getTarget (Proxy/getInvocationHandler l)))
           event-handlers (filter #(and (instance? Proxy %)
                                        (Proxy/isProxyClass (.getClass %))
                                        (instance? (-get-event-handler-target-class) (get-invocation-target %)))
                                  (.invoke get-listeners-method comp (into-array [])))
           id-target-map (reduce (fn [m p]
                                 (let [ehs (get-invocation-target p)]
                                   (assoc m (.getId ehs) ehs)))
                               {}
                               event-handlers)
           target-ids (apply hash-set (keys id-target-map))
           ;;
           ids (apply hash-set (map str (keys id-method-handler-map)))
           using-target-ids (cljset/difference ids target-ids)
           unused-target-ids (cljset/difference target-ids ids)]
       #_(lg "id-target-map:" id-target-map "\n"
             "using-target-ids:" using-target-ids "\n"
             "unused-target-ids:" unused-target-ids)
       ;; Assign handler to existing target.
       (doseq [[id target] id-target-map]
         (when-let [method-handler (or (get id-method-handler-map (symbol id)) (get id-method-handler-map id))]
           (let [[method handler] method-handler]
             (-set-handler-to-target handler target))))
       ;; Add new target for unset method.
       (doseq [id using-target-ids]
         (let [[method handler] (or (get id-method-handler-map (symbol id)) (get id-method-handler-map id))
               target (-set-handler-to-target handler (-create-event-handler-target id))
               ;; Use "" for the 4th arg to get whole event object on the handling method.
               event-handler (EventHandler/create listener-intf target "handleEvent" "" (when method (str method)))]
           ;; Clojure's .invoke is expecting args in array...
           (.invoke add-listener-method comp (into-array [event-handler]))))
       ;; Remove unused targets if requested.
       (when (and remove-unused-targets? (seq unused-target-ids))
         (doseq [event-handler event-handlers]
           (when (contains? unused-target-ids (.getId (get-invocation-target event-handler)))
             (.invoke remove-listener-method comp (into-array [event-handler]))))))))

(defn set-listener-handler-set
  "Given a seq of the pairs of listener-intf and id-method-handler-map, call
   set-listener-handlers for each pair with comp."
  [comp & listener-intf-id-method-handler-maps]
  (when-not (even? (count listener-intf-id-method-handler-maps))
    (throw (IllegalArgumentException. "set-listener-handler-set: odd listener-intf/id-method-handler-map")))
  (loop [pairs listener-intf-id-method-handler-maps]
    (when (seq pairs)
      (set-listener-handlers comp (first pairs) (second pairs))
      (recur (nnext pairs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-font
  ([props]
     (let [[name style size] (if (satisfies? PProperties props)
                               (let [name (get-prop props 'font.name "Courier")
                                     style (read-prop props 'font.style 'PLAIN)
                                     size (read-prop props 'font.size 11)]
                                 [name style size])
                               (let [sprops (get-props)
                                     name (get-prop sprops 'slix.font.name "Courier")
                                     style (read-prop sprops 'slix.font.style 'PLAIN)
                                     size (read-prop sprops 'slix.font.size 11)]
                                 [name style size]))]
       (create-font name style size)))
  ([name style size]
     (let [name (get-class-field-value Font name name)
           style (if (number? style)
                   style
                   (get-class-field-value Font style Font/PLAIN))
           size (if (number? size)
                  size
                  11)]
       (Font. name style size))))

(defn create-color
  [value]
  (if (and (vector? value) (< 2 (count value)))
    (Color. (nth value 0) (nth value 1) (nth value 2))
    (if-let [c (get-class-field-value Color (.toUpperCase (str value)))]
      c
      Color/WHITE)))

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

(defn- -setup-event-handler-target-class?
  []
  (let [event-handler-target-class (Class/forName (str (get-config 'src.sevenri.listeners.event-handler-target)))]
    (redef! *event-handler-target-class* event-handler-target-class)
    true))

;;;;

(defn startup-ui?
  []
  (apply while-each-true?
         (do-each-after* print-fn-name*
          -add-awt-event-listeners?
          -setup-event-handler-target-class?)))

(defn shutdown-ui?
  []
  (apply while-each-true?
         nil))
