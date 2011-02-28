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
  (:use [sevenri config defs event log refs])
  (:import (clojure.lang IProxy)
           (java.awt AWTEvent Color Component EventQueue Font Toolkit)
           (java.awt.event AWTEventListener FocusEvent KeyAdapter KeyEvent WindowEvent)
           (javax.swing BorderFactory JLabel PopupFactory)
           (javax.swing JInternalFrame JFrame)
           (java.util.logging Level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These fns are resolved at the startup time.
(using-fns ui slix
           [is-slix? slix-frame
            put-slix-prop get-slix-prop remove-slix-prop
            get-slixes close-slix get-slix-sevenri])

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
          (future
            (.show pup)
            (Thread/sleep (* 1000 sec))
            (.hide pup)))))))

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
  (let [dwlc (Class/forName (str (get-default :src :sevenri :listeners :defwinlistener)))
        dwl (.newInstance dwlc)]
    (.addWindowListener frame dwl)
    (.addWindowFocusListener frame dwl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let [dklc (Class/forName (str (get-default :src :sevenri :listeners :defkeylistener)))
        dkl (.newInstance dklc)]
    (.addKeyListener comp dkl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defn add-awt-event-listeners?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn shutdown-ui?
  []
  true)

(defn startup-ui?
  []
  (and true
       (add-awt-event-listeners?)))
