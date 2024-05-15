(defproject ubergraph "0.9.0"
  :description "Feature-loaded graph implementation"
  :url "http://github.com/engelberg/ubergraph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [aysylu/loom "1.0.2"]
                 [dorothy "0.0.7"]
                 [com.rpl/specter "1.1.4"]
                 [potemkin "0.4.6"]]
  :codox {:output-path "doc"
          :namespaces [ubergraph.core ubergraph.alg]
          :source-uri "http://github.com/Engelberg/ubergraph/tree/master/{filepath}#L{line}"}
  )
