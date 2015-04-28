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

(uber/pprint airports)

; What is the trip with the fewest hops from Artemis to Egglesberg?
(alg/pprint-path (alg/shortest-path airports {:start-node :Artemis, :end-node :Egglesberg}))
(alg/shortest-path airports :Artemis :Egglesberg)

; What is the trip that is the shortest distance from Coulton to Egglesberg?
(alg/shortest-path airports {:start-node :Coulton, :end-node :Egglesberg, :cost-attr :distance})
(alg/shortest-path airports :Coulton :Egglesberg :distance)

; What is the cheapest trip from Artemis to Egglesberg?
(alg/shortest-path airports :Artemis :Egglesberg :cost)

; Show me the cities I can get to from Artemis, in order from shortest distance trips to longest.
(alg/shortest-path airports {:start-node :Artemis, :traverse true})

; What are all the cities who are (at best) two hops from Egglesberg?
(alg/shortest-path airports {:start-node :Egglesberg, :traverse true, :min-cost 2, :max-cost 2})

; What is the fewest hops from Dentana to Egglesberg avoiding the airline AirLux?
(alg/shortest-path airports {:start-node :Dentana, :end-node :Egglesberg,
                             :edge-filter (fn [e] (not= :AirLux (uber/attr airports e :airline)))})

; What is the shortest distance from Egglesberg to Artemis, going only through large cities (population at least 3000)?
(alg/shortest-path airports {:start-node :Egglesberg, :end-node :Artemis,
                             :node-filter (fn [n] (<= 3000 (uber/attr airports n :population))),
                             :cost-attr :cost})

; What is the cheapest way to get from Coulton to any small city for a weekend getaway?
(alg/shortest-path airports {:start-node :Coulton, 
                             :end-node? (fn [n] (> 3000 (uber/attr airports n :population))),
                             :cost-attr :cost})

; I live halfway between Artemis and Balela and can use either airport.  What is the cheapest way
; to get from either of those airports to Dentana?
(alg/shortest-path airports {:start-nodes [:Artemis :Balela], :end-node :Dentana, :cost-attr :cost})

; My sister is coming to visit me from Dentana, which airport should she fly into to save money?
(alg/shortest-path airports {:start-node :Dentana, :end-nodes [:Artemis :Balela], :cost-attr :cost})

; What's the best way from Artemis to Egglesberg if my highest priority is fewest stops, and
; my second priority is price?
(alg/shortest-path airports {:start-node :Artemis, :end-node :Egglesberg, 
                             :cost-fn (fn [e] (+ 100000 (uber/attr airports e :cost)))}) 

; I'm planning to make a bunch of trips out of Coulton, let's build a table of all the shortest-distance
; paths with one traversal.
(def out-of-coulton (alg/shortest-path airports {:start-node :Coulton, :cost-attr :distance}))
(alg/path-to out-of-coulton :Artemis)


