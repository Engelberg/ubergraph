(defproject ubergraph "0.3.0"
  :description "Feature-loaded graph implementation"
  :url "http://github.com/engelberg/ubergraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [aysylu/loom "0.5.4"]
                 [dorothy "0.0.6"]
                 [potemkin "0.4.3"]]
  :codox {:output-path "doc"
          :namespaces [ubergraph.core ubergraph.alg]
          :source-uri "http://github.com/Engelberg/ubergraph/tree/master/{filepath}#L{line}"}
  )
