(ns ubergraph.examples
  (:require [clojure.test :refer :all]
            [ubergraph.core :as uber]))

(def graph1
  (uber/graph [:a :b] [:a :c] [:b :d]))

(def graph2
  (uber/graph [:a :b 2] [:a :c 3] [:b :d 4]))

(def graph3
  (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
              [:a :c {:weight 3 :cost 300 :distance 20}]))


