(defproject sevenri "0.2.0-SNAPSHOT"
  :description "Sevenri, a Clojure environment"
  :lic {:artifact :description
        :author ["Kei Suzuki"]
        :update true}
  :dependencies [;; base requirements
                 [org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [swank-clojure "1.2.1"]
                 ;; for NetBeans generated UI modules
                 [org.swinglabs/swing-layout "1.0.3"]]
  :dev-dependencies [[lein-lic "1.0.0"]]
  :main Sevenri)
