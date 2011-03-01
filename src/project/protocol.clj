;; I/F for slix; all takes a map with keys :slix-name, :name, and :arguments.
{:manager 'slix.planter ;; must be fully qualified
 :exists? 'exists?
 :built? 'built?
 :get-jars 'get-jars
 :build-and-run 'build-and-run
 }
