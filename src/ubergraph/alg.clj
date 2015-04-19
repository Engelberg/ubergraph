(ns ubergraph.alg
  (:require [ubergraph.core :as uber])
  (:import java.util.PriorityQueue java.util.HashMap java.util.LinkedList java.util.HashSet))

;; Various searches for shortest paths
;; For speed, on Java, use mutable queues and hash maps
;; Consider using Clojure data structures for better portability to Clojurescript

(defn- find-path
  ([to backlinks] (find-path to backlinks ()))
  ([to ^HashMap backlinks path]
    (if-let [prev-edge (.get backlinks to)]
      (recur (uber/src prev-edge) backlinks (cons prev-edge path))
      path)))

(defn- least-edges-path-helper [g goal? ^LinkedList queue ^HashMap backlinks]
  (loop []
    (if-let [node (.poll queue)]
      (if (goal? node)
        (find-path node backlinks)
        (do (doseq [edge (uber/out-edges g node)]
              (let [dst (uber/dest edge)]
                (when-not (.get backlinks dst)
                  (.add queue dst)
                  (.put backlinks dst edge))))
          (recur)))
      nil)))

(defn least-edges-path
  "Takes a graph g, a collection of starting nodes, and a goal? predicate. Returns
a path that gets you from one of the starting nodes to a node that satisfies the goal? predicate 
using the fewest possible edges."
  [g starting-nodes goal?]
  (let [queue (LinkedList.),
        backlinks (HashMap.)]
    (doseq [node starting-nodes]
      (.add queue node)
      (.put backlinks node nil))
    (least-edges-path g goal? queue backlinks)))

(defn- least-cost-path-helper [g goal? ^PriorityQueue queue ^HashMap least-costs 
                               ^HashMap backlinks cost-fn]
  (loop []
    (if-let [[cost-from-start-to-node node] (.poll queue)]
      (cond 
        (goal? node) (find-path node backlinks)
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)]
              (let [dst (uber/dest edge),
                    cost-from-node-to-dst (cost-fn edge),
                    cost-from-start-to-dst (+ cost-from-start-to-node 
                                              cost-from-node-to-dst)
                    least-cost-found-so-far-from-start-to-dst (.get least-costs dst)]
                (when (or (not least-cost-found-so-far-from-start-to-dst) 
                          (< cost-from-start-to-dst least-cost-found-so-far-from-start-to-dst))
                  (.add queue [cost-from-start-to-dst dst])
                  (.put least-costs dst cost-from-start-to-dst)
                  (.put backlinks dst edge))))
          (recur)))  
      nil)))

(defn least-cost-path
  "Takes a graph g, a collection of starting nodes, a goal? predicate, and optionally a cost function
(defaults to weight). Returns a list of edges that form a path with the least cost 
from one of the starting nodes to a node that satisfies the goal? predicate."  
  ([g starting-nodes goal?]
    (least-cost-path g starting-nodes goal? uber/weight))
  ([g starting-nodes goal? cost-fn]
    (let [least-costs (HashMap.),
        backlinks (HashMap.)
          queue (PriorityQueue.)]
      (doseq [node starting-nodes]
        (.put least-costs node 0)
        (.put backlinks node nil)
        (.add queue [0 node]))
      (least-cost-path-helper g goal? queue least-costs backlinks cost-fn))))

(defn- least-cost-path-with-heuristic-helper
  "AKA A* search"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn]
  (loop []
    (if-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
      (cond
        (goal? node) (find-path node backlinks)
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)]
              (let [dst (uber/dest edge),
                    cost-from-node-to-dst (cost-fn edge),
                    cost-from-start-to-dst (+ cost-from-start-to-node
                                              cost-from-node-to-dst)
                    least-cost-found-so-far-from-start-to-dst (.get least-costs dst)]
                (when (or (not least-cost-found-so-far-from-start-to-dst) 
                          (< cost-from-start-to-dst least-cost-found-so-far-from-start-to-dst))
                  (.add queue [(+ cost-from-start-to-dst (heuristic-fn dst))
                               [cost-from-start-to-dst dst]])
                  (.put least-costs dst cost-from-start-to-dst)
                  (.put backlinks dst edge))))
          (recur)))  
      nil)))

(defn least-cost-path-with-heuristic [g starting-nodes goal? cost-fn heuristic-fn]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes]
      (.put least-costs node 0)
      (.put backlinks node nil)
      (.add queue [(heuristic-fn node) [0 node]]))
    (least-cost-path-with-heuristic-helper g goal? queue least-costs cost-fn heuristic-fn)))

