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
                 [org.swinglabs/swing-layout "1.0.3"]
                 ;; for incantea
                 [incanter/incanter-core "1.2.3"]
                 [incanter/incanter-io "1.2.3"]
                 [incanter/incanter-charts "1.2.3"]
                 [incanter/incanter-processing "1.2.3"]
                 [incanter/incanter-mongodb "1.2.3"]
                 [incanter/incanter-pdf "1.2.3"]
                 [incanter/incanter-latex "1.2.3"]
                 [incanter/incanter-excel "1.2.3"]
                 ;; for documenter
                 [rhino/js "1.7R2"]]
  :dev-dependencies [[lein-lic "1.0.0"]]
  :main Sevenri)
