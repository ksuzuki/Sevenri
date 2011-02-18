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

(ns oplix.ced.fnclasses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *fn-classes*
  ;; [[:class set]+]
  [[:defines
    #{"defn" "defn-" "def" "def-" "defonce"
      "defmulti" "defmethod" "defmacro"
      "defstruct" "deftype" "defprotocol"
      "defrecord"
      "defalias" "defhinted" "defmacro-"
      "defn-memo" "defnk" "defonce-"
      "defstruct-" "defunbound" "defunbound-"
      "defvar" "defvar-"}]
   [:controls
    #{"let" "letfn" "do"
      "cond" "condp"
      "for" "loop" "recur"
      "when" "when-not" "when-let" "when-first"
      "if" "if-let" "if-not"
      "." ".." "->" "->>" "doto"
      "and" "or"
      "dosync" "doseq" "dotimes" "dorun" "doall"
      "load" "import" "unimport" "ns" "in-ns" "refer"
      "try" "catch" "finally" "throw"
      "with-open" "with-local-vars" "binding"
      "gen-class" "gen-and-load-class" "gen-and-save-class"
      "handler-case" "handle"}]
   [:lambda-expr
    #{"fn"}]
   [:core-fns
    #{"*" "*1" "*2" "*3" "*agent*"
      "*allow-unresolved-vars*" "*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*"
      "*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*"
      "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
      "*print-dup*" "*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
      "*read-eval*" "*source-path*" "*use-context-classloader*" "*warn-on-reflection*" "+"
      "-" "/"
      "<" "<=" "=" "==" ">"
      ">=" "accessor" "aclone"
      "agent" "agent-errors" "aget" "alength" "alias"
      "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
      "ancestors" "and" "apply" "areduce" "array-map"
      "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double"
      "aset-float" "aset-int" "aset-long" "aset-short" "assert"
      "assoc" "assoc!" "assoc-in" "associative?" "atom"
      "await" "await-for" "await1" "bases" "bean"
      "bigdec" "bigint" "binding" "bit-and" "bit-and-not"
      "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
      "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
      "boolean-array" "booleans" "bound-fn" "bound-fn*" "butlast"
      "byte" "byte-array" "bytes" "cast" "char"
      "char-array" "char-escape-string" "char-name-string" "char?" "chars"
      "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first"
      "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?"
      "clear-agent-errors" "clojure-version" "coll?" "comment" "commute"
      "comp" "comparator" "compare" "compare-and-set!" "compile"
      "complement" "concat" "cond" "condp" "conj"
      "conj!" "cons" "constantly" "construct-proxy" "contains?"
      "count" "counted?" "create-ns" "create-struct" "cycle"
      "dec" "decimal?" "declare" "definline" "defmacro"
      "defmethod" "defmulti" "defn" "defn-" "defonce"
      "defstruct" "delay" "delay?" "deliver" "deref"
      "derive" "descendants" "destructure" "disj" "disj!"
      "dissoc" "dissoc!" "distinct" "distinct?" "doall"
      "doc" "dorun" "doseq" "dosync" "dotimes"
      "doto" "double" "double-array" "doubles" "drop"
      "drop-last" "drop-while" "empty" "empty?" "ensure"
      "enumeration-seq" "eval" "even?" "every?"
      "extend" "extend-protocol" "extend-type" "extends?" "extenders"
      "false?" "ffirst" "file-seq" "filter" "find" "find-doc"
      "find-ns" "find-var" "first" "float" "float-array"
      "float?" "floats" "flush" "fn" "fn?"
      "fnext" "for" "force" "format" "future"
      "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?"
      "gen-class" "gen-interface" "gensym" "get" "get-in"
      "get-method" "get-proxy-class" "get-thread-bindings" "get-validator" "hash"
      "hash-map" "hash-set" "identical?" "identity" "if-let"
      "if-not" "ifn?" "import" "in-ns" "inc"
      "init-proxy" "instance?" "int" "int-array" "integer?"
      "interleave" "intern" "interpose" "into" "into-array"
      "ints" "io!" "isa?" "iterate" "iterator-seq"
      "juxt" "key" "keys" "keyword" "keyword?"
      "last" "lazy-cat" "lazy-seq" "let" "letfn"
      "line-seq" "list" "list*" "list?" "load"
      "load-file" "load-reader" "load-string" "loaded-libs" "locking"
      "long" "long-array" "longs" "loop" "macroexpand"
      "macroexpand-1" "make-array" "make-hierarchy" "map" "map?"
      "mapcat" "max" "max-key" "memfn" "memoize"
      "merge" "merge-with" "meta" "method-sig" "methods"
      "min" "min-key" "mod" "name" "namespace"
      "neg?" "newline" "next" "nfirst" "nil?"
      "nnext" "not" "not-any?" "not-empty" "not-every?"
      "not=" "ns" "ns-aliases" "ns-imports" "ns-interns"
      "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve"
      "ns-unalias" "ns-unmap" "nth" "nthnext" "num"
      "number?" "odd?" "or" "parents" "partial"
      "partition" "pcalls" "peek" "persistent!" "pmap"
      "pop" "pop!" "pop-thread-bindings" "pos?" "pr"
      "pr-str" "prefer-method" "prefers" "primitives-classnames" "print"
      "print-ctor" "print-doc" "print-dup" "print-method" "print-namespace-doc"
      "print-simple" "print-special-doc" "print-str" "printf" "println"
      "println-str" "prn" "prn-str" "promise" "proxy"
      "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super" "push-thread-bindings"
      "pvalues" "quot" "rand" "rand-int" "range"
      "ratio?" "rational?" "rationalize" "re-find" "re-groups"
      "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
      "read-line" "read-string" "reify" "reduce" "ref" "ref-history-count"
      "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure"
      "release-pending-sends" "rem" "remove" "remove-method" "remove-ns"
      "repeat" "repeatedly" "replace" "replicate"
      "require" "reset!" "reset-meta!" "resolve" "rest"
      "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq"
      "satisfies?" "second" "select-keys" "send" "send-off" "seq"
      "seq?" "seque" "sequence" "sequential?" "set"
      "set-validator!" "set?" "short" "short-array" "shorts"
      "shutdown-agents" "slurp" "some" "sort" "sort-by"
      "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?"
      "special-form-anchor" "special-symbol?" "split-at" "split-with" "str"
      "stream?" "string?" "struct" "struct-map" "subs"
      "subseq" "subvec" "supers" "swap!" "symbol"
      "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last"
      "take-nth" "take-while" "test" "the-ns" "time"
      "to-array" "to-array-2d" "trampoline" "transient" "tree-seq"
      "true?" "type" "unchecked-add" "unchecked-dec" "unchecked-divide"
      "unchecked-inc" "unchecked-multiply" "unchecked-negate" "unchecked-remainder" "unchecked-subtract"
      "underive" "unquote" "unquote-splicing" "update-in" "update-proxy"
      "use" "val" "vals" "var-get" "var-set"
      "var?" "vary-meta" "vec" "vector" "vector?"
      "when" "when-first" "when-let" "when-not" "while"
      "with-bindings" "with-bindings*" "with-in-str" "with-loading-context" "with-local-vars"
      "with-meta" "with-open" "with-out-str" "with-precision" "xml-seq"}]
   [:core-lib-fns
    #{ ;; clojure.inspector
      "atom?" "collection-tag" "get-child" "get-child-count" "inspect"
      "inspect-table" "inspect-tree" "is-leaf" "list-model" "list-provider"
      ;; clojure.main
      "load-script" "main" "repl" "repl-caught" "repl-exception"
      "repl-prompt" "repl-read" "skip-if-eol" "skip-whitespace" "with-bindings"
      ;; clojure.set
      "difference" "index" "intersection" "join" "map-invert"
      "project" "rename" "rename-keys" "select" "union"
      ;; clojure.stacktrace
      "e" "print-cause-trace" "print-stack-trace" "print-throwable" "print-trace-element"
      ;; clojure.template
      "do-template" "apply-template"
      ;; clojure.test
      "*initial-report-counters*" "*load-tests*" "*report-counters*" "*stack-trace-depth*" "*test-out*"
      "*testing-contexts*" "*testing-vars*" "are" "assert-any" "assert-expr"
      "assert-predicate" "compose-fixtures" "deftest" "deftest-" "file-position"
      "function?" "get-possibly-unbound-var" "inc-report-counter" "is" "join-fixtures"
      "report" "run-all-tests" "run-tests" "set-test" "successful?"
      "test-all-vars" "test-ns" "test-var" "testing" "testing-contexts-str"
      "testing-vars-str" "try-expr" "use-fixtures" "with-test" "with-test-out"
      ;; clojure.walk
      "keywordize-keys" "macroexpand-all" "postwalk" "postwalk-demo" "postwalk-replace"
      "prewalk" "prewalk-demo" "prewalk-replace" "stringify-keys" "walk"
      ;; clojure.xml
      "*current*" "*sb*" "*stack*" "*state*" "attrs"
      "content" "content-handler" "element" "emit" "emit-element"
      ;; clojure.zip
      "append-child" "branch?" "children" "down" "edit"
      "end?" "insert-child" "insert-left" "insert-right" "left"
      "leftmost" "lefts" "make-node" "next" "node"
      "path" "prev" "remove" "replace" "right"
      "rightmost" "rights" "root" "seq-zip" "up"}]
   [:deprecate-fns
    #{"add-watcher" "remove-watcher" "add-classpath"}]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *fnclass-props*
  ;; {:class {:color c, ...}, ...}
  {:defines {}
   :controls {}
   :lambda-expr {}
   :core-fns {}
   :core-lib-fns {}
   :deprecate-fns {}})
