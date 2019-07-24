(ns ubergraph.examples
  (:require [clojure.test :refer :all]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def graph1
  (uber/graph [:a :b] [:a :c] [:b :d]))

(def graph2
  (uber/graph [:a :b 2] [:a :c 3] [:b :d 4]))

(def graph3
  (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
              [:a :c {:weight 3 :cost 300 :distance 20}]))

(def graph4
  (uber/add-directed-edges graph2 [:a :d 8]))

(def graph5
  (uber/multidigraph [:a :b] [:a :b] [:b :a] [:a :c]))


(deftest test-equal-graphs?
  (are [x y] (= x y)
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
       
       (uber/graph [:a {:counter 1}])
       (uber/graph [:a {:counter 1}])       
       ))       

(deftest test-notequal-graphs?
  (let [g (uber/graph :a)]
    (is (not= (uber/set-attrs g :a {:counter 1})
              (uber/set-attrs g :a {:counter 2})))
    (is (not= (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
                          [:a :c {:weight 3 :cost 300 :distance 20}])
              (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
                          [:a :c {:weight 3 :cost 400 :distance 20}])))))

(deftest test-merge-attrs
  (is (= {:color :red, :n 2} 
         (uber/attrs (uber/add-edges (uber/graph "a" "b") ["a" "b" {:color :red}] ["b" "a" {:n 2}]) ["a" "b"])))
  (is (= true
         (= (uber/graph 1)
            (uber/add-attrs (uber/graph 1) 1 {})))))

;; successors and predecessors should not return duplicates, and
;; should return only nodes of the graph.
(defn no-duplicates? [coll]
  (or (empty? coll)
      (apply distinct? coll)))

(defn all-in-graph? [g coll]
  (every? #(uber/has-node? g %) coll))

(defn slow-successors [g n]
  (distinct (map uber/dest (uber/out-edges g n))))

(defn slow-predecessors [g n]
  (distinct (map uber/src (uber/in-edges g n))))

(defn all-good-succ-pred? [g msg]
  (doseq [n (uber/nodes g)]
    (is (= true (no-duplicates? (uber/successors g n)))
        (str msg " successors have duplicates"))
    (is (= true (no-duplicates? (uber/predecessors g n)))
        (str msg " predecessors have duplicates"))
    (is (= true (all-in-graph? g (uber/successors g n)))
        (str msg " successors not all nodes in graph"))
    (is (= true (all-in-graph? g (uber/predecessors g n)))
        (str msg " predecessors not all nodes in graph"))
    (is (= (set (uber/successors g n)) (set (slow-successors g n)))
        (str msg " successors not equal to slow-successors"))
    (is (= (set (uber/predecessors g n)) (set (slow-predecessors g n)))
        (str msg " predecessors not equal to slow-predecessors"))))

(deftest test-successors-predecessors
  (all-good-succ-pred? graph1 "graph1")
  (all-good-succ-pred? graph2 "graph2")
  (all-good-succ-pred? graph3 "graph3")
  (all-good-succ-pred? graph4 "graph4")
  (all-good-succ-pred? graph5 "graph5")
  (all-good-succ-pred? (uber/remove-edges graph1 [:b :d]) "graph1-edge-removed"))
