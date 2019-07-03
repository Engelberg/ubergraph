(ns ubergraph.core-test
  (:require [clojure.test :refer :all]
            [ubergraph.core :refer :all]))

(defn vec-edges [g]
  (for [e (edges g) :when (not (mirror-edge? e))] [(src e) (dest e)]))

(deftest simple-graph-test
  (let [g1 (graph [1 2] [1 3] [2 3] 4)
        g2 (graph {1 [2 3] 2 [3] 4 []})
        g3 (graph g1)
        g4 (graph g3 (digraph [5 6]) [7 8] 9)
        g5 (graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
        #{1 2 3 4} (set (nodes g1))
        #{[1 2] [1 3] [2 3]} (set (vec-edges g1))
        (set (nodes g2)) (set (nodes g1))
        (set (vec-edges g2)) (set (vec-edges g1))
        (set (nodes g3)) (set (nodes g1))
        (set (nodes g3)) (set (nodes g1))
        #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
        #{[1 2][1 3][2 3]
          [5 6] [7 8]} (set (vec-edges g4))
        #{} (set (nodes g5))
        #{} (set (edges g5))
        true (has-node? g1 4)
        true (has-edge? g1 1 2)
        false (has-node? g1 5)
        false (has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
        #{2 3} (set (successors g1 1))
        #{1 2} (set (successors g1 3))
        #{} (set (successors g1 4))
        2 (out-degree g1 1)
        2 (out-degree g1 3)
        0 (out-degree g1 4)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
        #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
        #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
        #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
        #{[1 2]} (set (vec-edges (add-edges g5 [1 2])))
        #{1 2} (set (nodes (remove-nodes g1 3 4)))
        #{[1 2]} (set (vec-edges (remove-nodes g1 3 4)))
        #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
        #{[2 3]} (set (vec-edges (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))))))

(deftest simple-digraph-test
  (let [g1 (digraph [1 2] [1 3] [2 3] 4)
        g2 (digraph {1 [2 3] 2 [3] 4 []})
        g3 (digraph g1)
        g4 (digraph g3 (graph [5 6]) [7 8] 9)
        g5 (digraph)
        g6 (transpose g1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
        #{1 2 3 4} (set (nodes g1))
        #{1 2 3 4} (set (nodes g6))
        #{[1 2] [1 3] [2 3]} (set (vec-edges g1))
        #{[2 1] [3 1] [3 2]} (set (vec-edges g6))
        (set (nodes g2)) (set (nodes g1))
        (set (vec-edges g2)) (set (vec-edges g1))
        (set (nodes g3)) (set (nodes g1))
        (set (nodes g3)) (set (nodes g1))
        #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
        #{[1 2] [1 3] [2 3] [5 6] [7 8]} (set (vec-edges g4))
        #{} (set (nodes g5))
        #{} (set (vec-edges g5))
        true (has-node? g1 4)
        true (has-edge? g1 1 2)
        false (has-node? g1 5)
        false (has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
        #{2 3} (set (successors g1 1))
        #{} (set (successors g1 3))
        #{} (set (successors g1 4))
        2 (out-degree g1 1)
        0 (out-degree g1 3)
        0 (out-degree g1 4)
        #{1 2} (set (predecessors g1 3))
        #{} (set (predecessors g1 1))
        2 (in-degree g1 3)
        0 (in-degree g1 1)
        #{1 2} (set (successors g6 3))
        #{} (set (successors g6 1))
        2 (out-degree g6 3)
        0 (out-degree g6 1)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
        #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
        #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
        #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
        #{[1 2]} (set (vec-edges (add-edges g5 [1 2])))
        #{1 2} (set (nodes (remove-nodes g1 3 4)))
        #{[1 2]} (set (vec-edges (remove-nodes g1 3 4)))
        #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [1 3])))
        #{[2 3]} (set (vec-edges (remove-edges g1 [1 2] [1 3])))))))

(deftest simple-weighted-graph-test
  (let [g1 (graph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (graph {1 {2 77 3 88} 2 {3 99} 4 []})
        g3 (graph g1)
        g4 (graph g3 (digraph [5 6 88]) [7 8] 9)
        g5 (graph)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
        #{1 2 3 4} (set (nodes g1))
        #{[1 2] [1 3] [2 3]} (set (vec-edges g1))
        (set (nodes g2)) (set (nodes g1))
        (set (vec-edges g2)) (set (vec-edges g1))
        (set (nodes g3)) (set (nodes g1))
        (set (nodes g3)) (set (nodes g1))
        #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
        #{[1 2] [1 3] [2 3]
          [5 6] [7 8]} (set (vec-edges g4))
        #{} (set (nodes g5))
        #{} (set (edges g5))
        true (has-node? g1 4)
        true (has-edge? g1 1 2)
        false (has-node? g1 5)
        false (has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
        #{2 3} (set (successors g1 1))
        #{1 2} (set (successors g1 3))
        #{} (set (successors g1 4))
        2 (out-degree g1 1)
        2 (out-degree g1 3)
        0 (out-degree g1 4)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
        #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
        #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
        #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
        #{[1 2]} (set (vec-edges (add-edges g5 [1 2])))
        #{1 2} (set (nodes (remove-nodes g1 3 4)))
        #{[1 2]} (set (vec-edges (remove-nodes g1 3 4)))
        #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
        #{[2 3]} (set (vec-edges (remove-edges g1 [1 2] [2 1] [1 3] [3 1])))))
    (testing "Weight"
      (are [expected got] (= expected got)
        77 (weight g1 1 2)
        77 (weight g2 1 2)
        77 (weight g3 1 2)
        1 (weight g4 6 5) ;This is different from Loom's behavior
        1 (weight g4 7 8)))))

(deftest simple-weighted-digraph-test
  (let [g1 (digraph [1 2 77] [1 3 88] [2 3 99] 4)
        g2 (digraph {1 {2 77 3 88} 2 {3 99} 4 []})
        g3 (digraph g1)
        g4 (digraph g3 (graph [5 6 88]) [7 8] 9)
        g5 (digraph)
        g6 (transpose g1)]
    (testing "Construction, nodes, edges"
      (are [expected got] (= expected got)
        #{1 2 3 4} (set (nodes g1))
        #{1 2 3 4} (set (nodes g6))
        #{[1 2] [1 3] [2 3]} (set (vec-edges g1))
        #{[2 1] [3 1] [3 2]} (set (vec-edges g6))
        (set (nodes g2)) (set (nodes g1))
        (set (vec-edges g2)) (set (vec-edges g1))
        (set (nodes g3)) (set (nodes g1))
        (set (nodes g3)) (set (nodes g1))
        #{1 2 3 4 5 6 7 8 9} (set (nodes g4))
        #{[1 2] [1 3] [2 3] [5 6] [7 8]} (set (vec-edges g4))
        #{} (set (nodes g5))
        #{} (set (edges g5))
        true (has-node? g1 4)
        true (has-edge? g1 1 2)
        false (has-node? g1 5)
        false (has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
        #{2 3} (set (successors g1 1))
        #{} (set (successors g1 3))
        #{} (set (successors g1 4))
        2 (out-degree g1 1)
        0 (out-degree g1 3)
        0 (out-degree g1 4)
        #{1 2} (set (predecessors g1 3))
        #{} (set (predecessors g1 1))
        2 (in-degree g1 3)
        0 (in-degree g1 1)
        #{1 2} (set (successors g6 3))
        #{} (set (successors g6 1))
        2 (out-degree g6 3)
        0 (out-degree g6 1)))
    (testing "Add & remove"
      (are [expected got] (= expected got)
        #{1 2 3 4 5} (set (nodes (add-nodes g1 5)))
        #{:a :b :c} (set (nodes (add-nodes g5 :a :b :c)))
        #{{:id 1} {:id 2}} (set (nodes (add-nodes g5 {:id 1} {:id 2})))
        #{[1 2]} (set (vec-edges (add-edges g5 [1 2])))
        #{1 2} (set (nodes (remove-nodes g1 3 4)))
        #{[1 2]} (set (vec-edges (remove-nodes g1 3 4)))
        #{1 2 3 4} (set (nodes (remove-edges g1 [1 2] [1 3])))
        #{[2 3]} (set (vec-edges (remove-edges g1 [1 2] [1 3])))))
    (testing "Weight"
      (are [expected got] (= expected got)
        77 (weight g1 1 2)
        77 (weight g2 1 2)
        77 (weight g3 1 2)
        77 (weight g6 2 1)
        88 (weight g4 6 5)
        1 (weight g4 7 8)))))

(deftest graph-with-self-cycles
  (let [g1 (graph [1 1])
        g2 (digraph [1 1])
        g3 (multigraph [1 1 {:color :red}] [1 1 {:color :blue}])
        g4 (multidigraph [1 1 {:color :red}] [1 1 {:color :blue}])]
    (testing "Undirected graph"
      (are [expected got] (= expected got)
        #{1} (set (successors g1 1))
        #{1} (set (predecessors g1 1))
        2 (out-degree g1 1)
        2 (in-degree g1 1)))
    (testing "Directed graph"
      (are [expected got] (= expected got)
        #{1} (set (successors g2 1))
        #{1} (set (predecessors g2 1))
        1 (out-degree g2 1)
        1 (in-degree g2 1)))
    (testing "Multigraph"
      (are [expected got] (= expected got)
        #{1} (set (successors g3 1))
        #{1} (set (predecessors g3 1))
        4 (out-degree g3 1)
        4 (in-degree g3 1)))
    (testing "Multidigraph"
      (are [expected got] (= expected got)
        #{1} (set (successors g4 1))
        #{1} (set (predecessors g4 1))
        2 (out-degree g4 1)
        2 (in-degree g4 1)))))

(deftest ubergraph-constructor
  (let [ug1 (ubergraph true true [1 2]),
        g1 (multigraph [1 2])
        ug2 (ubergraph true false [1 2]),
        g2 (multidigraph [1 2])
        ug3 (ubergraph false true [1 2])
        g3 (graph [1 2])
        ug4 (ubergraph false false [1 2])
        g4 (digraph [1 2])]
    (are [expected got] (= expected got)
         g1 ug1
         g2 ug2
         g3 ug3
         g4 ug4
         [true true false false] (map allow-parallel-edges? [ug1 ug2 ug3 ug4])
         [true false true false] (map undirected-graph? [ug1 ug2 ug3 ug4]))))

(deftest ubergraph-equality
  (are [expected got] (= expected got)
       true (= (graph [1 2]) (graph [1 2]))
       true (= (graph [1 2]) (graph [2 1]))
       false (= (graph [1 2]) (graph [1 3]))
       false (= (graph [1 2 {:a 1}]) (graph [1 2 {:a 2}]))
       true (= (graph [1 2 {:a 1}]) (graph [1 2 {:a 1}]))
       false (= (digraph [1 2 {:a 2}] [2 1 {:a 2}]) (graph [1 2 {:a 2}]))
       true (= (digraph [1 2 {:a 2}] [2 1 {:a 2}]) (digraph [1 2 {:a 2}] [2 1 {:a 2}]))
       true (= (multigraph [1 2 {:a 1}] [1 2 5])
               (multigraph [2 1 {:a 1}] [2 1 5]))
       false (= (multidigraph [1 2 {:a 1}] [1 2 5])
                (multidigraph [2 1 {:a 1}] [2 1 5]))
       true (= (digraph [0 1]) (digraph [0 1] [0 1]))
       true (= (digraph [0 1]) (add-directed-edges (digraph) [0 1] [0 1]))))

(defn- sorted-simple-edges [xs]
  (sort-by (juxt :src :dest) (map (fn [x] {:src (:src x) :dest (:dest x)}) xs)))

(defn- do-find-edges [g query]
  (sorted-simple-edges (find-edges g query)))

(defn- make-edges [& args]
  (map (fn [[src dest]] {:src src :dest dest}) (partition 2 args)))

(deftest find-edges-test
  (let [g0 (multidigraph)
        g1 (add-edges g0 [:a :b {:type "local"}])
        g2 (add-edges g1 [:a :b {:type "local"}])
        g3 (add-edges g2 [:a :b {:type "global"}])
        g4 (add-edges g3 [:x :y {:type "global"}])
        g5 (add-edges g4 [:a :y {:type "global"}])
        g6 (add-edges g5 [:x :y {:type "global" :position 5}])]

    (testing "finds edges by attribute"
      (is (= (make-edges :a :b)
             (do-find-edges g1 {:src :a :dest :b :type "local"})))
      (is (= (make-edges :a :b :a :b)
             (do-find-edges g2 {:src :a :dest :b :type "local"})))
      (is (= (make-edges :a :b :a :b)
             (do-find-edges g3 {:src :a :dest :b :type "local"})))
      (is (= (make-edges :a :b)
             (do-find-edges g3 {:src :a :dest :b :type "global"})))
      (is (= (make-edges :a :b :a :y)
             (do-find-edges g5 {:src :a :type "global"})))
      (is (= (make-edges :a :b :a :b)
             (do-find-edges g5 {:dest :b :type "local"})))
      (is (= (make-edges :a :b)
             (do-find-edges g5 {:dest :b :type "global"})))
      (is (= (make-edges :a :b :a :y :x :y)
             (do-find-edges g5 {:type "global"})))
      (is (= (make-edges :x :y)
             (do-find-edges g6 {:position 5})))
      (is (= (make-edges :x :y)
             (do-find-edges g6 {:type "global" :position 5}))))))

(deftest github-issue-38
  (let [g1 (multidigraph [1 {:label "n1"}]
                         [2 {:label "n2"}]
                         [1 2 {:label "edge12"}])
        g2 (remove-nodes g1 2)
        g3 (add-nodes g2 2)]
    (is (= (attrs g1 2) {:label "n2"}))
    ;; There should be no attributes for a node that was removed, then
    ;; added back again without any attributes.
    (is (= (attrs g3 2) {}))))

(deftest github-issue-38-edges
  ;; This was already behaving correctly before Github issue #38 was
  ;; filed.  Adding a test case for the extra assurance and catching
  ;; future possible regressions.
  (let [g1 (multidigraph [1 {:label "n1"}]
                         [2 {:label "n2"}]
                         [1 2 {:label "edge12"}])
        g2 (remove-edges g1 [1 2])
        g3 (add-edges g2 [1 2])]
    (is (= (attrs g1 [1 2]) {:label "edge12"}))
    (is (= (attrs g2 [1 2]) {}))
    ;; There should be no attributes for an edge that was removed,
    ;; then added back again without any attributes.
    (is (= (attrs g3 [1 2]) {}))))
