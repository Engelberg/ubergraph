(ns ubergraph.alg-test
  (:use clojure.test)
  (:require [ubergraph.alg :as alg]
            [ubergraph.core :as uber]            
            [loom.graph :as l]
            [loom.alg :as la]))

;; http://fr.wikipedia.org/wiki/Algorithme_de_Dijkstra
(def g4
  (l/weighted-graph
   [:a :b 85]
   [:b :f 80]
   [:f :i 250]
   [:i :j 84]
   [:a :c 217]
   [:c :g 186]
   [:c :h 103]
   [:d :h 183]
   [:h :j 167]
   [:a :e 173]
   [:e :j 502]))

(def g4u
  (uber/graph
   [:a :b 85]
   [:b :f 80]
   [:f :i 250]
   [:i :j 84]
   [:a :c 217]
   [:c :g 186]
   [:c :h 103]
   [:d :h 183]
   [:h :j 167]
   [:a :e 173]
   [:e :j 502]))


(deftest breadth-first-test
  (are [expected got] (= expected got)
       ;'#{1 2 3 5 6 7} (set (bf-traverse g7))
       ;#{1 2 3} (set (bf-traverse g7 1))
       ;#{1 2 3 4 5 6 7 8} (set (bf-traverse g8))
       ;#{1 2 3 4 5 6 7 8} (set (nodes (digraph (bf-span g8))))
       ;#{2 3} (set (successors (digraph (bf-span g6)) 1))
       ;false (not (some #{(bf-traverse (remove-nodes g6 5))}
       ;                 [[0 1 2 3 4] [0 1 3 2 4]]))
       ;#{:r} (set (bf-traverse g2 :r :when #(< %3 1)))
       ;#{:r :o :b :g} (set (bf-traverse g2 :r :when #(< %3 2)))
       ;#{:r :o :b :g :p} (set (bf-traverse g2 :r :when #(< %3 3)))
       [:a :e :j] (alg/nodes-in-path (alg/shortest-path g4 [:a] #{:j}))
       [:a :e :j] (alg/nodes-in-path (alg/shortest-path g4u [:a] #{:j}))
       [:a :c :h :j] (la/shortest-path g4u :a :j)
       [:a :c :h :j] (alg/nodes-in-path (alg/shortest-path g4u [:a] #{:j} {:cost-fn #(uber/weight g4u %)}))
       [:a :c :h :j] (alg/nodes-in-path (alg/shortest-path g4 [:a] #{:j} {:cost-fn #(uber/weight g4 %)}))
       ))       

