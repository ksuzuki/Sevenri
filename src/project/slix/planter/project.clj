(defproject planter "0.1.0"
  :description "Lein deps for lein47ri"
  :dependencies [[org.apache.ant/ant "1.7.1"]
                 [org.apache.ant/ant-nodeps "1.7.1"]
                 [robert/hooke "1.1.0"]
                 [org.apache.maven/maven-ant-tasks "2.0.10" :exclusions [ant]]])
