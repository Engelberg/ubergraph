(defproject ubergraph "0.1.2"
  :description "Feature-loaded graph implementation"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [aysylu/loom "0.5.0"]
                 [dorothy "0.0.6"]
                 [potemkin "0.3.13"]]
  :codox {:include [ubergraph.core ubergraph.alg]
          :src-dir-uri "http://github.com/Engelberg/ubergraph/tree/master/"
          :src-linenum-anchor-prefix "L"}
  )
