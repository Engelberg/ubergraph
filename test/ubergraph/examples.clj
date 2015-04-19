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

(def graph4
  (uber/add-directed-edges graph2 [:a :d 8]))


(deftest test-equal-graphs?
  (are [x y] (uber/equal-graphs? x y)
       (uber/graph [:a :b] [:a :c] [:b :d]) (uber/graph [:a :b] [:a :c] [:b :d])
       (uber/graph [:a :b 2] [:a :c 3] [:b :d 4]) (uber/graph [:a :b 2] [:a :c 3] [:b :d 4])
       
       (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
                   [:a :c {:weight 3 :cost 300 :distance 20}])
       (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
              [:a :c {:weight 3 :cost 300 :distance 20}])
       
       (uber/multigraph [:a :b] [:a :b] [:a :b {:color :red}])
       (uber/multigraph [:a :b] [:a :b {:color :red}] [:a :b])
       
       (uber/add-attr graph1 :a :b :weight 3)
       (uber/graph [:a :b 3] [:a :c] [:b :d])
       
       (uber/remove-edges graph1 [:b :d])
       (uber/graph [:a :b] [:a :c] :d)
       
       (uber/remove-edges graph1 [:d :b])
       (uber/graph [:a :b] [:a :c] :d)
       
       ))       