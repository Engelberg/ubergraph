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
    


