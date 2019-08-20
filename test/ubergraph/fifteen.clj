(ns ubergraph.fifteen
  (:require  [clojure.test :refer :all]
             [ubergraph.core :as uber]
             [ubergraph.alg :as alg]
             [com.rpl.specter :refer :all]))

;; A board state is a 4x4 vector of numbers 1-15 and \- (grid) and coord of empty slot
(defrecord State [grid slot])

;; Constructing a state

(defn find-slot "Finds coord of empty slot" [grid]
  (first (for [i (range 4), j (range 4)
               :when (= ((grid i) j) \-)]
           [i j])))

(defn make-state "Constructs State from grid" [grid]
  (State. grid (find-slot grid)))

(def solved-state (make-state [[1 2 3 4]
                               [5 6 7 8]
                               [9 10 11 12]
                               [13 14 15 \-]]))

;; Transitions out from a given State to other States
;; We label the transitions with what number is moved, and in what direction

(defn valid-coord? [[row col]]
  (and (<= 0 row 3) (<= 0 col 3)))

(defn neighbors [[row col]]
  (filter valid-coord? [[(inc row) col :up]
                        [(dec row) col :down]
                        [row (inc col) :left]
                        [row (dec col) :right]]))

(defn transitions [{:keys [grid slot]}]
  (let [ns (neighbors slot)
        [slot-row slot-col] slot]
    (for [[row col dir] ns
          :let [num ((grid row) col)
                new-grid (multi-transform
                          (multi-path [(nthpath row col) (terminal-val \-)]
                                      [(nthpath slot-row slot-col)
                                       (terminal-val num)])
                          grid)]]
      {:dest (State. new-grid [row col]), :number num :direction dir})))

;; A simple lower-bound heuristic for A* search

(defn reverse-index [grid]
  (into {} (for [i (range 4), j (range 4)] [((grid i) j) [i j]])))

(def reverse-solved-grid (reverse-index (:grid solved-state)))

(defn abs [n] (if (neg? n) (- n) n))
(defn taxi-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn lower-bound [{:keys [grid]}]
  (let [lookup (reverse-index grid)]
    (apply +
           (for [n (range 1 16)]
             (taxi-distance (get lookup n) (get reverse-solved-grid n))))))

(defn solve [grid]
  (alg/shortest-path transitions {:start-node (make-state grid),
                                  :end-node solved-state,
                                  :heuristic-fn lower-bound}))

(def sample1 [[1 2 3 \-]
              [5 6 7 8]
              [9 10 11 12]
              [13 14 15 4]])

(deftest test-solver
  (are [x y] (= y x)
    (mapv peek (alg/edges-in-path (solve sample1)))
    [{:number 3, :direction :right}
     {:number 7, :direction :up}
     {:number 8, :direction :left}
     {:number 12, :direction :up}
     {:number 4, :direction :up}
     {:number 15, :direction :right}
     {:number 11, :direction :down}
     {:number 8, :direction :down}
     {:number 12, :direction :left}
     {:number 4, :direction :up}
     {:number 8, :direction :right}
     {:number 12, :direction :down}
     {:number 7, :direction :down}
     {:number 3, :direction :left}
     {:number 4, :direction :up}
     {:number 8, :direction :up}
     {:number 12, :direction :right}
     {:number 11, :direction :up}
     {:number 15, :direction :left}]))

;; Linear conflict heuristic

(defn linear-conflicts [row expected])  

;; A better heuristic

(def solved-heuristic-state (State. [[4 0 0 0] [0 4 0 0] [0 0 4 0] [0 0 0 3]] 3))

(defn heuristic-transitions [{:keys [grid slot]}]
  (for [row (filter #(<= 0 % 3) [(inc slot) (dec slot)]),
        col (range 4),
        :when (pos? ((grid row) col))
        :let [new-grid (multi-transform
                        (multi-path [(nthpath row col) (terminal dec)]
                                    [(nthpath slot col) (terminal inc)])
                        grid)]]
    {:dest (State. new-grid row)}))

(def reachable-heuristic-states
  (into {}
        (map (juxt alg/end-of-path alg/cost-of-path))
        (alg/shortest-path heuristic-transitions
                           {:start-node solved-heuristic-state :traverse true})))

(let [rows [#{1 2 3 4} #{5 6 7 8} #{9 10 11 12} #{13 14 15}]]
  (defn better-lower-bound [{:keys [grid slot]}]
    (reachable-heuristic-states
     (State. (vec (for [row (range 4)]
                    (vec (for [nums rows]
                           (count (filter nums (grid row)))))))
             (slot 0)))))

;; Pattern database

(def char-map {0 \0 1 \1 2 \2 3 \3 4 \4 5 \5 6 \6 7 \7 8 \8 9 \9 10 \A 11 \B 12 \C 13 \D 14 \E 15 \F \- \-})
(defn state->str [{:keys [grid]}]
  (apply str (sequence (comp cat (map char-map)) grid)))
  
(def pattern1 (State. [[1 0 0 0] [5 6 0 0] [9 10 0 0] [13 0 0 \-]] [3 3]))
(def pattern2 (State. [[0 2 3 4] [0 0 0 0] [0 0 0 0] [0 0 0 \-]] [3 3]))
(def pattern3 (State. [[0 0 0 0] [0 0 7 8] [0 0 11 12] [0 14 15]] [3 3]))

(defn pattern-transitions [{:keys [grid slot]}]
  (let [ns (neighbors slot)
        [slot-row slot-col] slot]
    (for [[row col dir] ns
          :let [num ((grid row) col)
                new-grid (multi-transform
                          (multi-path [(nthpath row col) (terminal-val \-)]
                                      [(nthpath slot-row slot-col)
                                       (terminal-val num)])
                          grid)]]
      {:dest (State. new-grid [row col]), :number num :direction dir,
       :weight (if (zero? num) 0 1)})))

(defn reachable-pattern-states [pattern]
  (into {}
        (map (juxt (comp state->str alg/end-of-path) alg/cost-of-path))
        (alg/shortest-path pattern-transitions
                           {:start-node pattern :traverse true
                            :cost-attr :weight})))

(defn generate-pattern-dbs []
  (def pattern1-db (reachable-pattern-states pattern1))
  (def pattern2-db (reachable-pattern-states pattern2))
  (def pattern3-db (reachable-pattern-states pattern3)))

(defn solve-faster [grid]
  (alg/shortest-path transitions {:start-node (make-state grid),
                                  :end-node solved-state,
                                  :heuristic-fn (fn [s] (max (lower-bound s)
                                                             (better-lower-bound s)))}))

(def hardest-puzzle [[\- 12 9 13]
                     [15 11 10 14]
                     [3 7 2 5]
                     [4 8 6 1]])







