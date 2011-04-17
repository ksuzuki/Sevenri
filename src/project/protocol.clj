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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query-project interface
;;
;; Sevenri communicates with a project manager through this query interface.
;; Sevenri figures out the manager's name (a slix name), the I/F namespace,
;; and query entry points for query keywords.
;;
;; All the manager's query handler fns take a map with these keys;
;; :slix-name, :name, and :arguments.

{:slix 'planter ;; name of slix providing this manager I/F
 :manager 'slix.planter.manager ;; I/F ns - must be fully qualified
 ;; query keywords for manager
 :ready? 'ready? ;; is manager ready?
 :setup? 'setup? ;; setup manager to make it ready to work
 :shutdown 'shutdown ;; shutdown manager
 ;; query keywords for project
 :exists? 'exists? ;; does project exist?
 :built? 'built? ;; is project built?
 :build? 'build? ;; build project
 :get-jars 'get-jars ;; built project jar
 :build-and-run 'build-and-run ;; build project and run slix requiring it
 }
