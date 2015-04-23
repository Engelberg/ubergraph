(ns ubergraph.alg
  (:require [ubergraph.core :as uber]
            [ubergraph.protocols :as prots]
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
   start-of-path
   end-of-path
   path-to
   path-between])

(declare find-path)

(defrecord Path [list-of-edges cost end]
  ubergraph.protocols/IPath
  (edges-in-path [this] @list-of-edges)
  (nodes-in-path [this] (when (seq @list-of-edges)
                          (cons (uber/src (first @list-of-edges))
                                (map uber/dest @list-of-edges))))
  (cost-of-path [this] cost)
  (end-of-path [this] end)
  (start-of-path [this] (first (nodes-in-path this))))

(defrecord AllPathsFromSource [^HashMap backlinks ^HashMap least-costs]
  ubergraph.protocols/IAllPathsFromSource
  (path-to [this dest]
    (->Path (delay (find-path dest backlinks))
            (.get least-costs dest)
            dest)))

(defrecord AllBFSPathsFromSource [^HashMap backlinks ^HashMap depths]
  ubergraph.protocols/IAllPathsFromSource
  (path-to [this dest]
    (->Path (delay (find-path dest backlinks))
            (.get depths dest)
            dest)))
            
(defrecord AllPaths [all-paths]
  ubergraph.protocols/IAllPaths
  (path-between [this src dest]
    (path-to (get all-paths src) dest)))

(alter-meta! #'->Path assoc :no-doc true)
(alter-meta! #'->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'->AllPaths assoc :no-doc true)
(alter-meta! #'map->Path assoc :no-doc true)
(alter-meta! #'map->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'map->AllPaths assoc :no-doc true)

(extend-type nil
  ubergraph.protocols/IPath
  (edges-in-path [this] nil)
  (nodes-in-path [this] nil)
  (cost-of-path [this] nil)
  (start-of-path [this] nil)
  (end-of-path [this] nil)
  ubergraph.protocols/IAllPaths
  (path-to [this dest] nil)
  ubergraph.protocols/IAllPathsFromSource
  (path-between [this src dest] nil))     

(def no-goal (with-meta #{} {:no-goal true})) ; Used to search all possibilities

(defn- find-path
  ([to backlinks] (find-path to backlinks ()))
  ([to ^HashMap backlinks path]
    (let [prev-edge (.get backlinks to)]
      (if (= prev-edge ())
        path
        (recur (uber/src prev-edge) backlinks (cons prev-edge path))))))

(defn- least-edges-path-helper [g goal? ^LinkedList queue ^HashMap backlinks ^HashMap depths node-filter edge-filter]
  (loop []
    (if-let [node (.poll queue)]
      (let [depth (.get depths node)]        
        (if (goal? node)
          (->Path (delay (find-path node backlinks)) depth node)
          (do
            (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)]
              (let [dst (uber/dest edge)
                    inc-depth (inc depth)]
                (when (and (node-filter node) (not (.get backlinks dst)))
                  (.add queue dst)
                  (.put depths dst inc-depth)
                  (.put backlinks dst edge))))
            (recur))))
      (if (identical? no-goal goal?)
        (->AllBFSPathsFromSource backlinks depths)
        nil))))

(defn- least-edges-path
  "Takes a graph g, a collection of starting nodes, and a goal? predicate. Returns
a path that gets you from one of the starting nodes to a node that satisfies the goal? predicate 
using the fewest possible edges."
  [g starting-nodes goal? node-filter edge-filter]
  (let [queue (LinkedList.),
        backlinks (HashMap.)
        depths (HashMap.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node)
                                           (node-filter node))]
      (.add queue node)
      (.put depths node 0)
      (.put backlinks node ()))
    (least-edges-path-helper g goal? queue backlinks depths node-filter edge-filter)))

(defn- least-cost-path-helper [g goal? ^PriorityQueue queue ^HashMap least-costs 
                               ^HashMap backlinks cost-fn node-filter edge-filter]
  (loop []
    (if-let [[cost-from-start-to-node node] (.poll queue)]
      (cond 
        (goal? node) (->Path (delay (find-path node backlinks)) (.get least-costs node) node)
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)
                    :when (node-filter (uber/dest edge))]
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
  [g starting-nodes goal? cost-fn node-filter edge-filter]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [0 node]))
    (least-cost-path-helper g goal? queue least-costs backlinks cost-fn node-filter edge-filter)))

(defn- least-cost-path-with-heuristic-helper
  "AKA A* search"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn node-filter edge-filter]
  (loop []
    (if-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
      (cond
        (goal? node) (->Path (find-path node backlinks) (.get least-costs node))
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)
                    :when (node-filter (uber/dest edge))]
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
  [g starting-nodes goal? cost-fn heuristic-fn node-filter edge-filter]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [(heuristic-fn node) [0 node]]))
    (least-cost-path-with-heuristic-helper g goal? queue least-costs cost-fn heuristic-fn node-filter edge-filter)))

(defn shortest-path 
  "Finds the shortest path in graph g from a collection of starting nodes to
   any node that satisfies the goal? predicate.  Returns an implementation of
   the IPath protocol (edges-in-path, nodes-in-path, cost-of-path).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :cost-attr - Alternatively, can specify an edge attribute to use as the cost
   :heuristic-fn - A function that takes a node as an input and returns a
             lower-bound on the distance to a goal node, used to guide the search
             and make it more efficient.
   :node-filter - A predicate function that takes a node and returns true or false.
             If specified, only nodes that pass this node-filter test will be considered in the search.
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g starting-nodes goal?] (shortest-path g starting-nodes goal? {}))
  ([g starting-nodes goal? options-map]
    (assert (not (and (get options-map :cost-fn) 
                      (get options-map :cost-attr))) 
            "Can't specify both a :cost-fn and a :cost-attr")
    (let [cost-attr (get options-map :cost-attr)
          cost-fn (if cost-attr
                    #(uber/attr g % cost-attr)
                    (get options-map :cost-fn))
          heuristic-fn (get options-map :heuristic-fn)
          node-filter (get options-map :node-filter (constantly true))
          edge-filter (get options-map :edge-filter (constantly true))]
      (assert (coll? starting-nodes) "starting-nodes must be a collection")
      (assert (ifn? goal?) "goal? must be a function")
      (cond
        (and (nil? cost-fn) (nil? cost-attr) (nil? heuristic-fn))
        (least-edges-path g starting-nodes goal? node-filter edge-filter),
                        
        heuristic-fn
        (least-cost-path-with-heuristic
          g starting-nodes goal? (if cost-fn cost-fn (constantly 1)) heuristic-fn node-filter edge-filter),
        
        :else
        (least-cost-path g starting-nodes goal? cost-fn node-filter edge-filter)))))
                
(defn shortest-paths-from 
  "Finds the shortest path in graph g from a collection of starting nodes to
   every other reachable node.  Returns an implementation of the 
   IAllPathsFromSource protocol (edges-in-path-to, nodes-in-path-to, cost-of-path-to).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :cost-attr - Alternatively, can specify an edge attribute to use as the cost
             and make it more efficient.
   :node-filter - A predicate function that takes a node and returns true or false.
             If specified, only nodes that pass this node-filter test will be considered in the search.
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g starting-nodes] (shortest-paths-from g starting-nodes {}))
  ([g starting-nodes options-map]
    (assert (not (and (get options-map :cost-fn) 
                      (get options-map :cost-attr))) 
            "Can't specify both a :cost-fn and a :cost-attr")    
    (let [goal? no-goal
          cost-attr (get options-map :cost-attr)
          cost-fn (if cost-attr
                    #(uber/attr g % cost-attr)
                    (get options-map :cost-fn))
          node-filter (get options-map :node-filter (constantly true))
          edge-filter (get options-map :edge-filter (constantly true))]
      (assert (coll? starting-nodes) "starting-nodes must be a collection")
      (assert (ifn? goal?) "goal? must be a function")
      (cond
        (nil? cost-fn)
        (least-edges-path g starting-nodes goal? node-filter edge-filter),
        
        :else
        (least-cost-path g starting-nodes goal? cost-fn node-filter edge-filter)))))

(defn shortest-paths 
  "Finds the shortest path in graph g between every pair of nodes.  
   Returns an implementation of the IAllPaths protocol 
   (edges-in-path-between, nodes-in-path-between, cost-of-path-between).

   Optionally takes an options-map which can contain the following entries:
   :cost-fn - A function that takes an edge as an input and returns a cost 
             (defaults to every edge having a cost of 1)
   :cost-attr - Alternatively, can specify an edge attribute to use as the cost
             and make it more efficient.
   :node-filter - A predicate function that takes a node and returns true or false.
             If specified, only nodes that pass this node-filter test will be considered in the search.
   :edge-filter - A predicate function that takes an edge and returns true or false.
             If specified, only edges that pass this edge-filter test will be considered in the search.
"
  ([g] (shortest-paths g {}))
  ([g options-map]
    (->AllPaths (into {} (for [node (uber/nodes g)]
                           (shortest-paths-from g node options-map))))))
