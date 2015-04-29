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

(def negative-weight-example
  (uber/digraph
    :f
    [:s :a 5]
    [:s :c -2]
    [:c :a 2]
    [:c :d 3]
    [:a :b 1]
    [:b :c 2]
    [:b :d 7]
    [:b :t 3]
    [:d :t 10]))

(deftest bellman-ford-test
  (is (= {:a 0, :c -2, :d 1, :b 1, :t 4, :s 0}
         (:least-costs (alg/bellman-ford negative-weight-example :s :weight))))
  (is (= {:e 173, :g 403, :c 217, :j 487, :h 320, :b 85, :d 503, :f 165, :i 415, :a 0}
         (:least-costs (alg/bellman-ford g4u :a :weight)))))
  

(deftest breadth-first-test
  (are [expected got] (= expected got)
       [:a :e :j] (alg/nodes-in-path (alg/shortest-path g4 {:start-node :a :end-node :j}))
       [:a :e :j] (alg/nodes-in-path (alg/shortest-path g4u {:start-node :a :end-node :j}))
       [:a :c :h :j] (la/shortest-path g4u :a :j)
       [:a :c :h :j] (alg/nodes-in-path 
                       (alg/shortest-path g4u 
                                          {:start-node :a,
                                           :end-node :j,
                                           :cost-attr :weight})),
       [:a :c :h :j] (alg/nodes-in-path 
                       (alg/shortest-path g4 
                                          {:start-node :a,
                                           :end-node :j,
                                           :cost-fn #(uber/weight g4 %)}))
       ))       

(def words
  (-> (clojure.java.io/resource "sgb-words.txt")
    clojure.java.io/reader
    line-seq))

(defn all-changes-of-word
  "Returns a sequence of words altered by one letter, and distance"
  [word]
  (let [v (vec word)]
    (for [i (range (count v)),
          letter "abcdefghijklmnopqrstuvwxyz"
          :when (not= letter (v i))]
      [(apply str (assoc v i letter)) (Math/abs (- (int (v i)) (int letter)))])))

(defn word-letter-distance [w1 w2]
  (apply + (for [[l1 l2] (map vector w1 w2)]
             (Math/abs (- (int l1) (int l2))))))

(defn word-edit-distance [w1 w2]
  (apply + (for [[l1 l2] (map vector w1 w2),
                 :when (not= l1 l2)]
             1)))

(defn word-graph []
  (let [words   (-> (clojure.java.io/resource "sgb-words.txt")
                  clojure.java.io/reader
                  line-seq),
        word-set (set words),
        graph (apply uber/graph words),
        graph (uber/add-edges* 
                graph
                (for [word words,
                      [adjacent-word distance] (all-changes-of-word word),
                      :when (and (pos? (compare word adjacent-word))
                                 (contains? word-set adjacent-word))]
                  [word adjacent-word distance]))
        graph (reduce (fn [g [i word]] (uber/add-attr g word :rank i))
                      graph (map-indexed vector words))]
    graph))
        
(defn random-word-test [wg words]
  (let [word1 (rand-nth words),
        word2 (rand-nth words),
        
        bf-path-loom (la/bf-path wg word1 word2),
        bf-path-uber (alg/nodes-in-path (alg/shortest-path wg {:start-node word1,
                                                               :end-node word2}))
        bf-path-astar (alg/nodes-in-path (alg/shortest-path wg {:start-node word1,
                                                                :end-node word2,
                                                                :heuristic-fn #(word-edit-distance % word2)}))
        short-path-uber (alg/shortest-path wg {:start-node word1,
                                               :end-node word2})
        short-path-astar (alg/shortest-path wg {:start-node word1,
                                                :end-node word2,
                                                :heuristic-fn #(word-edit-distance % word2)})]
    (is (= bf-path-loom bf-path-uber))
    (is (= (first bf-path-uber)
           (first bf-path-astar)))
    (is (= (last bf-path-uber)
           (last bf-path-astar)))
    (is (= (count bf-path-uber)
           (count bf-path-astar)))
    (is (= (alg/start-of-path short-path-uber)
           (alg/start-of-path short-path-astar)))
    (is (= (alg/end-of-path short-path-uber)
           (alg/end-of-path short-path-astar)))
    (is (= (alg/cost-of-path short-path-uber)
           (alg/cost-of-path short-path-astar)))))
        
(deftest random-word-tests
  (let [wg (word-graph) words (vec words)]
    (dotimes [i 50]
      (random-word-test wg words))))
    


(def airports
  (-> (uber/multigraph        
        [:Artemis :Balela {:color :blue, :airline :CheapAir, :cost 200, :distance 40}]
        [:Artemis :Balela {:color :green, :airline :ThriftyLines, :cost 167, :distance 40}]
        [:Artemis :Coulton {:color :green, :airline :ThriftyLines, :cost 235, :distance 120}]
        [:Artemis :Dentana {:color :blue, :airline :CheapAir, :cost 130, :distance 160}]
        [:Balela :Coulton {:color :green, :airline :ThriftyLines, :cost 142, :distance 70}]
        [:Balela :Egglesberg {:color :blue, :airline :CheapAir, :cost 350, :distance 50}])
    (uber/add-directed-edges 
      [:Dentana :Egglesberg {:color :red, :airline :AirLux, :cost 80, :distance 50}]
      [:Egglesberg :Coulton {:color :red, :airline :AirLux, :cost 80, :distance 30}]
      [:Coulton :Dentana {:color :red, :airline :AirLux, :cost 80, :distance 65}])
    (uber/add-attr :Artemis :population 3000)
    (uber/add-attr :Balela :population 2000)
    (uber/add-attr :Coulton :population 4000)
    (uber/add-attr :Dentana :population 1000)
    (uber/add-attr :Egglesberg :population 5000)))

(def ^:private airport-edge-details (juxt uber/src uber/dest #(uber/attr airports % :airline)))

(defn- airport-edges [path]
  (map airport-edge-details (alg/edges-in-path path)))


;Need to figure out how to make these tests more robust, because the specific paths among equal possibilities
;can vary from one run to the next 
(deftest test-airports    
  (are [actual expected] (= expected actual)
       (alg/cost-of-path (alg/shortest-path airports {:start-node :Artemis, :end-node :Egglesberg}))
       2,  
       (alg/cost-of-path (alg/shortest-path airports :Coulton :Egglesberg :distance))       
       115
       (alg/cost-of-path (alg/shortest-path airports :Artemis :Egglesberg :cost))
       210
       (alg/cost-of-path (alg/shortest-path airports {:start-node :Artemis, :end-node :Egglesberg, 
                                                      :cost-fn (fn [e] (+ 100000 (uber/attr airports e :distance)))}))
       200090
       (alg/cost-of-path (alg/path-to (alg/shortest-path airports {:start-node :Coulton, :cost-attr :distance}) :Artemis))
       110
       (frequencies (map alg/cost-of-path (alg/shortest-path airports {:start-node :Artemis, :traverse true})))
       {0 1, 1 3, 2 1}
       (set (map alg/end-of-path (alg/shortest-path airports {:start-node :Egglesberg, :traverse true, :min-cost 2, :max-cost 2})))
       #{:Dentana :Artemis}
       (alg/cost-of-path (alg/shortest-path airports {:start-node :Dentana, :end-node :Egglesberg,
                                               :edge-filter (fn [e] (not= :AirLux (uber/attr airports e :airline)))}))
       3
       (alg/cost-of-path (alg/shortest-path airports {:start-node :Egglesberg, :end-node :Artemis,
                                                      :node-filter (fn [n] (<= 3000 (uber/attr airports n :population))),
                                                      :cost-attr :cost}))
       315
       (alg/cost-of-path (alg/shortest-path airports {:start-node :Coulton, 
                                                      :end-node? (fn [n] (> 3000 (uber/attr airports n :population))),
                                                      :cost-attr :cost}))
       80
        (alg/cost-of-path (alg/shortest-path airports {:start-nodes [:Artemis :Balela], :end-node :Dentana, :cost-attr :cost}))
        130
        (airport-edges (alg/shortest-path airports {:start-node :Dentana, :end-nodes [:Artemis :Balela], :cost-attr :cost}))
        '([:Dentana :Artemis :CheapAir])
       ))


