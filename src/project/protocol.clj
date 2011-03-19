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

;; query-project I/F; all takes a map with keys :slix-name, :name, and :arguments.

{:manager 'slix.planter.manager ;; must be fully qualified
 ;; query for manager
 :ready? 'ready?
 :setup? 'setup?
 :shutdown 'shutdown
 ;; query for project
 :exists? 'exists?
 :built? 'built?
 :build? 'build?
 :get-jars 'get-jars
 :build-and-run 'build-and-run
 }
