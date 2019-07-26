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

(deftest equality-still-true-after-caching-hash
  (let [g1 (graph [(Integer. -1) (Integer. -2)])
        g2 (graph [(Long. -1) (Long. -2)])]
    ;; No cached hashes calculated yet, so ignored by this =
    ;; comparison, which returns true
    ;; because (= (Integer. -1) (Long. -1)) is true in Clojure.  Those
    ;; values were selected because clojure.core/= is true between
    ;; them, but Java .hashCode is different between them.
    ;; clojure.core/hash is the same for both.
    (is (= g1 g2))
    ;; Force hashes to be calculated and cached.
    (is (integer? (hash g1)))
    (is (integer? (hash g2)))
    ;; This will use calculated hashes during =, but should still be
    ;; true.
    (is (= g2 g1))))


;; Attribute-related functions:
;; attr
;; attrs
;; add-attr
;; add-attrs
;; set-attrs (there is no set-attr function)
;; remove-attr
;; remove-attrs
(deftest attributes-on-nodes-tests
  (testing "Access node attrs with existing node"
    (let [g1 (graph [1 {:foo 17}] [2 {:bar 28}])
          g2 (add-attr g1 1 :foo 18)
          g3 (add-attr g1 1 :baz 29)
          g4 (add-attrs g1 1 {:baz 29 :guh 30})
          g5 (set-attrs g1 1 {:baz 29 :guh 30})
          g6 (remove-attr g1 1 :foo)
          g7 (remove-attr g1 1 :baz)
          g8 (remove-attrs g1 1 [:foo :bar])
          g9 (remove-attrs g1 1 [:guh :bar])]
      (is (= 17 (attr g1 1 :foo)))
      (is (= {:foo 17} (attrs g1 1)))
      ;; add-attr and add-attrs do 'merge' of attr maps
      (is (= {:foo 18} (attrs g2 1)))
      (is (= {:foo 17 :baz 29} (attrs g3 1)))
      (is (= {:foo 17 :baz 29 :guh 30} (attrs g4 1)))
      ;; whereas set-attr replaces entire attr map
      (is (= {:baz 29 :guh 30} (attrs g5 1)))
      (is (= {} (attrs g6 1)))
      ;; Trying to remove non-existent attr keys leaves attrs
      ;; unchanged
      (is (= {:foo 17} (attrs g7 1)))
      (is (= {} (attrs g8 1)))
      (is (= {:foo 17} (attrs g9 1)))))
  (testing "Access node attrs with non-existent node"
    (let [g1 (graph [1 {:foo 17}] [2 {:bar 28}])]
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (attr g1 3 :foo)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (attrs g1 3)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (add-attr g1 3 :foo 18)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (add-attrs g1 3 {:foo 18})))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (set-attrs g1 3 {:foo 18})))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (remove-attr g1 3 :foo)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (remove-attrs g1 3 [:foo]))))))


(defn test-attrs-existing-edge-separate-args [n1 n2 undirected-graph?
                                              reverse-for-op? reverse-for-read?]
  (let [g1 (if undirected-graph?
             (graph [n1 n2 {:foo 17}])
             (digraph [n1 n2 {:foo 17}]))
        [op-n1 op-n2] (if reverse-for-op?
                        [n2 n1]
                        [n1 n2])
        [read-n1 read-n2] (if reverse-for-op?
                            [n2 n1]
                            [n1 n2])
        g2 (add-attr g1 op-n1 op-n2 :foo 18)
        g3 (add-attr g1 op-n1 op-n2 :baz 29)
        g4 (add-attrs g1 op-n1 op-n2 {:baz 29 :guh 30})
        g5 (set-attrs g1 op-n1 op-n2 {:baz 29 :guh 30})
        g6 (remove-attr g1 op-n1 op-n2 :foo)
        g7 (remove-attr g1 op-n1 op-n2 :baz)
        g8 (remove-attrs g1 op-n1 op-n2 [:foo :bar])
        g9 (remove-attrs g1 op-n1 op-n2 [:guh :bar])]
    (is (= 17 (attr g1 read-n1 read-n2 :foo)))
    (is (= {:foo 17} (attrs g1 read-n1 read-n2)))
    ;; add-attr and add-attrs do 'merge' of attr maps
    (is (= {:foo 18} (attrs g2 read-n1 read-n2)))
    (is (= {:foo 17 :baz 29} (attrs g3 read-n1 read-n2)))
    (is (= {:foo 17 :baz 29 :guh 30} (attrs g4 read-n1 read-n2)))
    ;; whereas set-attr replaces entire attr map
    (is (= {:baz 29 :guh 30} (attrs g5 read-n1 read-n2)))
    (is (= {} (attrs g6 read-n1 read-n2)))
    ;; Trying to remove non-existent attr keys leaves attrs
    ;; unchanged
    (is (= {:foo 17} (attrs g7 read-n1 read-n2)))
    (is (= {} (attrs g8 read-n1 read-n2)))
    (is (= {:foo 17} (attrs g9 read-n1 read-n2)))))


(defn test-attrs-nonexisting-edge-separate-args [undirected-graph?]
  (let [g1 (if undirected-graph?
             (graph [1 2 {:foo 17}])
             (digraph [1 2 {:foo 17}]))]
    (doseq [[op-n1 op-n2] (if undirected-graph?
                            [[1 3]]
                            ;; For a directed graph, edge [2 1] should
                            ;; not exist, because we only added the
                            ;; edge with direction [1 2] above.
                            [[1 3] [2 1]])]
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (attr g1 op-n1 op-n2 :foo)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (attrs g1 op-n1 op-n2)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (add-attr g1 op-n1 op-n2 :foo 18)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (add-attrs g1 op-n1 op-n2 {:baz 29 :guh 30})))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (set-attrs g1 op-n1 op-n2 {:baz 29 :guh 30})))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (remove-attr g1 op-n1 op-n2 :foo)))
      (is (thrown-with-msg? IllegalArgumentException
                            #"Invalid node or edge description"
                            (remove-attrs g1 op-n1 op-n2 [:foo :bar]))))))


(defn test-attrs-existing-edge-object [n1 n2 undirected-graph?
                                       reverse-for-op? reverse-for-read?]
  (let [g1 (if undirected-graph?
             (graph [n1 n2 {:foo 17}])
             (digraph [n1 n2 {:foo 17}]))
        forward-edge (find-edge g1 n1 n2)
        backward-edge (if undirected-graph?
                        (find-edge g1 n2 n1))
        op-edge (if reverse-for-op? backward-edge forward-edge)
        read-edge (if reverse-for-op? backward-edge forward-edge)
        g2 (add-attr g1 op-edge :foo 18)
        g3 (add-attr g1 op-edge :baz 29)
        g4 (add-attrs g1 op-edge {:baz 29 :guh 30})
        g5 (set-attrs g1 op-edge {:baz 29 :guh 30})
        g6 (remove-attr g1 op-edge :foo)
        g7 (remove-attr g1 op-edge :baz)
        g8 (remove-attrs g1 op-edge [:foo :bar])
        g9 (remove-attrs g1 op-edge [:guh :bar])]

    (is (= false (mirror-edge? forward-edge)))
    (when undirected-graph?
      (is (= true (mirror-edge? backward-edge))))

    (is (= 17 (attr g1 read-edge :foo)))
    (is (= {:foo 17} (attrs g1 read-edge)))
    ;; add-attr and add-attrs do 'merge' of attr maps
    (is (= {:foo 18} (attrs g2 read-edge)))
    (is (= {:foo 17 :baz 29} (attrs g3 read-edge)))
    (is (= {:foo 17 :baz 29 :guh 30} (attrs g4 read-edge)))
    ;; whereas set-attr replaces entire attr map
    (is (= {:baz 29 :guh 30} (attrs g5 read-edge)))
    (is (= {} (attrs g6 read-edge)))
    ;; Trying to remove non-existent attr keys leaves attrs
    ;; unchanged
    (is (= {:foo 17} (attrs g7 read-edge)))
    (is (= {} (attrs g8 read-edge)))
    (is (= {:foo 17} (attrs g9 read-edge)))))


(defn test-attrs-nonexisting-edge-object [undirected-graph?]
  (let [g1 (if undirected-graph?
             (graph [1 2 {:foo 17}])
             (digraph [1 2 {:foo 17}]))
        g2 (if undirected-graph?
             (graph [1 2 {:foo 17}])
             (digraph [1 2 {:foo 17}]))
        op-edge (find-edge g2 1 2)]
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (attr g1 op-edge :foo)))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (attrs g1 op-edge)))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (add-attr g1 op-edge :foo 18)))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (add-attrs g1 op-edge {:baz 29 :guh 30})))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (set-attrs g1 op-edge {:baz 29 :guh 30})))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (remove-attr g1 op-edge :foo)))
    (is (thrown-with-msg? IllegalArgumentException
                          #"edge does not exist in graph g"
                          (remove-attrs g1 op-edge [:foo :bar])))))


(deftest attributes-on-edges-tests
  (testing "undirected edge attrs, edges exist, specified as src dest using separate args"
    (let [undirected-graph? true]
      (doseq [reverse-for-op? [false true]
              reverse-for-read? [false true]]
        (test-attrs-existing-edge-separate-args 1 2 undirected-graph?
                                                reverse-for-op?
                                                reverse-for-read?))))
  (testing "directed edge attrs, edges exist, specified as src dest using separate args"
    (let [undirected-graph? false
          reverse-for-op? false
          reverse-for-read? false]
      (test-attrs-existing-edge-separate-args 1 2 undirected-graph?
                                              reverse-for-op?
                                              reverse-for-read?)))
  (testing "edge attrs, edges do not exist, specified as src dest using separate args"
    (doseq [undirected-graph? [false true]]
      (test-attrs-nonexisting-edge-separate-args undirected-graph?)))

  ;; TBD: Note that the behavior with non-existent edges is
  ;; _different_ (at least as of ubergraph version 0.6.1) if you
  ;; specify it via a vector of [src dest], vs. making the attribute
  ;; function call using separate args to specify src and dest.
  ;; Consider harmonizing these behaviors in a future version of
  ;; ubergraph, e.g. perhaps by making the [src dest] calls also throw
  ;; an IllegalArgumentException.

  (testing "undirected edge attrs, edges exist, specified as edge object"
    (let [undirected-graph? true]
      (doseq [reverse-for-op? [false true]
              reverse-for-read? [false true]]
        (test-attrs-existing-edge-object 1 2 undirected-graph?
                                         reverse-for-op?
                                         reverse-for-read?))))
  (testing "directed edge attrs, edges exist, specified as edge object"
    (let [undirected-graph? false
          reverse-for-op? false
          reverse-for-read? false]
      (test-attrs-existing-edge-object 1 2 undirected-graph?
                                       reverse-for-op?
                                       reverse-for-read?)))
  (testing "edge attrs, edges do not exist, specified as edge object"
    (doseq [undirected-graph? [false true]]
      (test-attrs-nonexisting-edge-object undirected-graph?))))
