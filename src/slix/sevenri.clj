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

(ns ^{:slix true :singleton true}
  slix.sevenri
  (:use [sevenri event props slix])
  (:use [slix.sevenri aotlist lists ui])
  (:require [slix.sevenri public]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; slix event handlers

(defn frame-created
  [event]
  (create-sevenri-frame))

(defn opened
  [event]
  (initialize)
  (set-slix-visible)
  (set-divider)
  (update-sn-list)
  (aot-compile-slixes))

(defn closing
  [event]
  (when-not (read-public-prop (slix-public) 'can.close)
    (event-response-donot-close)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn slix-created
  [event]
  (update-sn-list))

(defn slix-opened
  [event]
  (update-sn-list))

(defn slix-closed
  [event]
  (update-sn-list))

(defn slix-deleted
  [event]
  (update-sn-list))

(defn slix-purged
  [event]
  (update-sn-list))

;;;;

(defn slixes-opening
  [event]
  (show-slixes-event-cursor true))

(defn slixes-opened
  [event]
  (show-slixes-event-cursor false))

(defn slixes-closing
  [event]
  (show-slixes-event-cursor true))

(defn slixes-closed
  [event]
  (show-slixes-event-cursor false))
