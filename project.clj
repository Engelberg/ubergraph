(defproject ubergraph "0.2.0"
  :description "Feature-loaded graph implementation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [aysylu/loom "0.5.4"]
                 [dorothy "0.0.6"]
                 [potemkin "0.4.3"]]
  :codox {:include [ubergraph.core ubergraph.alg]
          :src-dir-uri "http://github.com/Engelberg/ubergraph/tree/master/"
          :src-linenum-anchor-prefix "L"}
  )
