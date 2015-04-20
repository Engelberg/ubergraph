(ns ubergraph.alg
  (:require [ubergraph.core :as uber]
            [potemkin :refer [import-vars]])
  (:import java.util.PriorityQueue java.util.HashMap java.util.LinkedList java.util.HashSet))

;; Various searches for shortest paths
;; For speed, on Java, use mutable queues and hash maps
;; Consider using Clojure data structures for better portability to Clojurescript

(import-vars
  [ubergraph.protocols
   ; Path protocols
   edges-in-path
   nodes-in-path
   cost-of-path
   edges-in-path-to
   nodes-in-path-to
   cost-of-path-to
   edges-in-path-between
   nodes-in-path-between
   cost-of-path-between])

(declare find-path)

(defrecord Path [list-of-edges cost]
  IPath
  (edges-in-path [this] list-of-edges)
  (nodes-in-path [this] (when (seq list-of-edges)
                          (cons (uber/src (first list-of-edges))
                                (map uber/dest list-of-edges))))
  (cost-of-path [this] cost))

(defrecord AllPathsFromSource [^HashMap backlinks ^HashMap least-costs]
  IAllPathsFromSource
  (edges-in-path-to [this dest] (find-path dest backlinks))
  (nodes-in-path-to [this dest] (let [list-of-edges (find-path dest backlinks)]
                                  (when (seq list-of-edges)
                                    (cons (uber/src (first list-of-edges))
                                          (map uber/dest list-of-edges)))))
  (cost-of-path-to [this dest] (.get least-costs dest)))

(defrecord AllBFSPathsFromSource [^HashMap backlinks]
  IAllPathsFromSource
  (edges-in-path-to [this dest] (find-path dest backlinks))
  (nodes-in-path-to [this dest] (let [list-of-edges (find-path dest backlinks)]
                                  (when (seq list-of-edges)
                                    (cons (uber/src (first list-of-edges))
                                          (map uber/dest list-of-edges)))))
  (cost-of-path-to [this dest] (let [list-of-edges (find-path dest backlinks)]
                                 (when (seq list-of-edges)
                                   (count list-of-edges)))))

(defrecord AllPaths [all-paths]
  IAllPaths
  (edges-in-path-between [this src dest]
    (edges-in-path-to (get all-paths src) dest))
  (nodes-in-path-between [this src dest]
    (nodes-in-path-to (get all-paths src) dest))
  (cost-of-path-between [this src dest]
    (cost-of-path-to (get all-paths src) dest)))

(alter-meta! #'->Path assoc :no-doc true)
(alter-meta! #'->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'->AllPaths assoc :no-doc true)
(alter-meta! #'map->Path assoc :no-doc true)
(alter-meta! #'map->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'map->AllPaths assoc :no-doc true)

(extend-type nil
  IPath
  (edges-in-path [this] nil)
  (nodes-in-path [this] nil)
  (cost-of-path [this] nil)
  IAllPaths
  (edges-in-path-to [this dest] nil)
  (nodes-in-path-to [this dest] nil)
  (cost-of-path-to [this dest] nil)
  IAllPathsFromSource
  (edges-in-path-between [this src dest] nil)
  (nodes-in-path-between [this src dest] nil)
  (cost-of-path-between [this src dest] nil))
     

(def no-goal (with-meta #{} {:no-goal true})) ; Used to search all possibilities

(defn- find-path
  ([to backlinks] (find-path to backlinks ()))
  ([to ^HashMap backlinks path]
    (if-let [prev-edge (.get backlinks to)]
      (recur (uber/src prev-edge) backlinks (cons prev-edge path))
      path)))

(defn- least-edges-path-helper [g goal? ^LinkedList queue ^HashMap backlinks edge-filter]
  (loop []
    (if-let [node (.poll queue)]
      (if (goal? node)
        (let [path (find-path node backlinks)]
          (->Path path (count path)))
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)]
              (let [dst (uber/dest edge)]
                (when-not (.get backlinks dst)
                  (.add queue dst)
                  (.put backlinks dst edge))))
          (recur)))
      (if (identical? no-goal goal?)
        (->AllBFSPathsFromSource backlinks)
        nil))))

(defn- least-edges-path
  "Takes a graph g, a collection of starting nodes, and a goal? predicate. Returns
a path that gets you from one of the starting nodes to a node that satisfies the goal? predicate 
using the fewest possible edges."
  [g starting-nodes goal? edge-filter]
  (let [queue (LinkedList.),
        backlinks (HashMap.)]
    (doseq [node starting-nodes]
      (.add queue node)
      (.put backlinks node nil))
    (least-edges-path g goal? queue backlinks edge-filter)))

(defn- least-cost-path-helper [g goal? ^PriorityQueue queue ^HashMap least-costs 
                               ^HashMap backlinks cost-fn edge-filter]
  (loop []
    (if-let [[cost-from-start-to-node node] (.poll queue)]
      (cond 
        (goal? node) (->Path (find-path node backlinks) (.get least-costs node))
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)]
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
      (if (identical? no-goal goal?)
        (->AllPathsFromSource backlinks least-costs)
        nil))))

(defn- least-cost-path
  "Takes a graph g, a collection of starting nodes, a goal? predicate, and optionally a cost function
(defaults to weight). Returns a list of edges that form a path with the least cost 
from one of the starting nodes to a node that satisfies the goal? predicate."  
  [g starting-nodes goal? cost-fn edge-filter]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes]
      (.put least-costs node 0)
      (.put backlinks node nil)
      (.add queue [0 node]))
    (least-cost-path-helper g goal? queue least-costs backlinks cost-fn edge-filter)))

(defn- least-cost-path-with-heuristic-helper
  "AKA A* search"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn edge-filter]
  (loop []
    (if-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
      (cond
        (goal? node) (->Path (find-path node backlinks) (.get least-costs node))
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)]
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
      (if (identical? no-goal goal?)
        (->AllPathsFromSource backlinks least-costs)
        nil))))

(defn- least-cost-path-with-heuristic
  "Heuristic function must take a single node as an input, and return
   a lower bound of the cost from that node to a goal node"
  [g starting-nodes goal? cost-fn heuristic-fn edge-filter]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes]
      (.put least-costs node 0)
      (.put backlinks node nil)
      (.add queue [(heuristic-fn node) [0 node]]))
    (least-cost-path-with-heuristic-helper g goal? queue least-costs cost-fn heuristic-fn edge-filter)))

(defn shortest-path 
  "Finds the shortest path in graph g from a collection of starting nodes to
   any node that satisfies the goal? predicate.  Returns an implementation of
   the IPath protocol (edges-in-path, nodes-in-path, cost-of-path).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :heuristic-fn - A function that takes a node as an input and returns a
             lower-bound on the distance to a goal node, used to guide the search
             and make it more efficient.
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g starting-nodes goal?] (shortest-path g starting-nodes goal? {}))
  ([g starting-nodes goal? options-map]
    (let [cost-fn (get options-map :cost-fn)
          heuristic-fn (get options-map :heuristic-fn)
          edge-filter (get options-map :edge-filter (constantly true))]
      (cond
        (and (nil? cost-fn) (nil? heuristic-fn))
        (least-edges-path g starting-nodes goal? edge-filter),
        
        heuristic-fn
        (least-cost-path-with-heuristic
          g starting-nodes goal? (if cost-fn cost-fn (constantly 1)) heuristic-fn edge-filter),
        
        :else
        (least-cost-path g starting-nodes goal? cost-fn edge-filter)))))
                
(defn shortest-paths-from 
  "Finds the shortest path in graph g from a collection of starting nodes to
   every other reachable node.  Returns an implementation of the 
   IAllPathsFromSource protocol (edges-in-path-to, nodes-in-path-to, cost-of-path-to).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g starting-nodes] (shortest-paths-from g starting-nodes {}))
  ([g starting-nodes options-map]
    (let [goal? no-goal
          cost-fn (get options-map :cost-fn)          
          edge-filter (get options-map :edge-filter (constantly true))]
      (cond
        (nil? cost-fn)
        (least-edges-path g starting-nodes goal? edge-filter),
        
        :else
        (least-cost-path g starting-nodes goal? cost-fn edge-filter)))))

(defn shortest-paths 
  "Finds the shortest path in graph g between every pair of nodes.  
   Returns an implementation of the IAllPaths protocol 
   (edges-in-path-between, nodes-in-path-between, cost-of-path-between).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g] (shortest-paths g {}))
  ([g options-map]
    (->AllPaths (into {} (for [node (uber/nodes g)]
                           (shortest-paths-from g node options-map))))))
