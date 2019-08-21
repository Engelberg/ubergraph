(ns ubergraph.eight
  (:require  [clojure.test :refer :all]
             [ubergraph.core :as uber]
             [ubergraph.alg :as alg]
             [com.rpl.specter :refer :all]))

;; A board state is a 3x3 vector of numbers 1-8 and \- (grid) and coord of empty slot
(defrecord State [grid slot])

;; Constructing a state

(defn find-slot "Finds coord of empty slot" [grid]
  (first (for [i (range 3), j (range 3)
               :when (= ((grid i) j) \-)]
           [i j])))

(defn make-state "Constructs State from grid" [grid]
  (State. grid (find-slot grid)))

(def solved-state (make-state [[1 2 3]
                               [4 5 6]
                               [7 8 \-]]))

;; Transitions out from a given State to other States
;; We label the transitions with what number is moved, and in what direction

(defn valid-coord? [[row col]]
  (and (<= 0 row 2) (<= 0 col 2)))

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
  (into {} (for [i (range 3), j (range 3)] [((grid i) j) [i j]])))

(def reverse-solved-grid (reverse-index (:grid solved-state)))

(defn abs [n] (if (neg? n) (- n) n))
(defn taxi-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn lower-bound [{:keys [grid]}]
  (let [lookup (reverse-index grid)]
    (apply +
           (for [n (range 1 9)]
             (taxi-distance (get lookup n) (get reverse-solved-grid n))))))

(defn solve [grid]
  (mapv peek
        (alg/edges-in-path
         (alg/shortest-path transitions {:start-node (make-state grid),
                                         :end-node solved-state,
                                         :heuristic-fn lower-bound}))))

(def sample1 [[\- 1 3]
              [4 2 5]
              [7 8 6]])

(deftest test-solver
  (are [x y] (= y x)
    (solve sample1)
    [{:number 1, :direction :left}
     {:number 2, :direction :up}
     {:number 5, :direction :left}
     {:number 6, :direction :up}]))

(def hardest [[8 6 7]
              [2 5 4]
              [3 \- 1]])

;; Want to go faster? - Let's find all solutions
;; We do that by searching out from the goal state

(def flip-direction {:down :up, :up :down, :left :right, :right :left})
(defn reverse-solution [sol]
  (into [] (map (fn [s] (update s :direction flip-direction)))
        (rseq sol)))

;; First time you use this it will take longer while generating all solutions.
(let [all-paths (delay (alg/shortest-path transitions {:start-node solved-state}))]
  (defn solve-faster [grid]
    (->> (alg/path-to @all-paths (make-state grid))
         alg/edges-in-path
         (mapv peek)
         reverse-solution)))

;; Generating random puzzles, to better compare heuristics

(defn count-inversions [permutation]
  (let [lookup 
        (into {} (for [i (range 9)] [(permutation i) i]))]
    (count (for [i (range 1 9), j (range (inc i) 9)
                 :when (< (lookup j) (lookup i))]
             1))))

(defn solvable? [permutation]
  (even? (count-inversions permutation)))

(defn generate-random-solvable-grid []
  (let [permutation (shuffle [1 2 3 4 5 6 7 8 \-])]
    (if (solvable? permutation)
      (mapv vec (partition 3 permutation))
      (generate-random-solvable-grid))))

(defn benchmark [n]
  (solve-faster sample1) ;; to generate pattern databases
  (let [grids (repeatedly n generate-random-solvable-grid)]
    (time (run! solve grids))
    (time (run! solve-faster grids))))





