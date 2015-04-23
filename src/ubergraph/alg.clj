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
;   path-between  Reserved for future use in all-paths algorithms
   ])

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
            
(alter-meta! #'->Path assoc :no-doc true)
(alter-meta! #'->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'map->Path assoc :no-doc true)
(alter-meta! #'map->AllPathsFromSource assoc :no-doc true)

(extend-type nil
  ubergraph.protocols/IPath
  (edges-in-path [this] nil)
  (nodes-in-path [this] nil)
  (cost-of-path [this] nil)
  (start-of-path [this] nil)
  (end-of-path [this] nil)
  ubergraph.protocols/IAllPathsFromSource
  (path-to [this dest] nil))     

(def ^:private no-goal (with-meta #{} {:no-goal true})) ; Used to search all possibilities

(defn- find-path
  "Work backwards from the destination to reconstruct the path"
  ([to backlinks] (find-path to backlinks ()))
  ([to ^HashMap backlinks path]
    (let [prev-edge (.get backlinks to)]
      (if (= prev-edge ())
        path
        (recur (uber/src prev-edge) backlinks (cons prev-edge path))))))

(defn- least-edges-path-helper
  "Find the path with the least number of edges"
  [g goal? ^LinkedList queue ^HashMap backlinks ^HashMap depths node-filter edge-filter]
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

(defn- least-edges-path-seq-helper
  "Variation that produces a seq of paths produced during the traversal"
  [g goal? ^LinkedList queue ^HashMap backlinks ^HashMap depths node-filter edge-filter]
  (let [stepfn 
        (fn stepfn [] 
          (when-let [node (.poll queue)]
            (let [depth (.get depths node)]
              (cons (->Path (delay (find-path node backlinks)) depth node)
                    (lazy-seq
                      (if (goal? node)
                        nil
                        (do
                          (doseq [edge (uber/out-edges g node)
                                  :when (edge-filter edge)]
                            (let [dst (uber/dest edge)
                                  inc-depth (inc depth)]
                              (when (and (node-filter node) (not (.get backlinks dst)))
                                (.add queue dst)
                                (.put depths dst inc-depth)
                                (.put backlinks dst edge))))
                          (stepfn))))))))]

    (stepfn)))

(defn- least-edges-path
  "Takes a graph g, a collection of starting nodes, and a goal? predicate. Returns
a path that gets you from one of the starting nodes to a node that satisfies the goal? predicate 
using the fewest possible edges."
  [g starting-nodes goal? node-filter edge-filter traverse?]
  (let [queue (LinkedList.),
        backlinks (HashMap.)
        depths (HashMap.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node)
                                           (node-filter node))]
      (.add queue node)
      (.put depths node 0)
      (.put backlinks node ()))
    (if traverse?
      (least-edges-path-seq-helper g goal? queue backlinks depths node-filter edge-filter)
      (least-edges-path-helper g goal? queue backlinks depths node-filter edge-filter))))

(defn- least-cost-path-helper
  "Find the shortest path with respect to the cost-fn applied to edges"
  [g goal? ^PriorityQueue queue ^HashMap least-costs 
   ^HashMap backlinks cost-fn node-filter edge-filter]
  (loop []
    (if-let [[cost-from-start-to-node node] (.poll queue)]
      (cond 
        (goal? node) (->Path (delay (find-path node backlinks)) (.get least-costs node) node)
        (> cost-from-start-to-node (.get least-costs node)) (recur)
        :else
        (do (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)
                    :let [dst (uber/dest edge)]
                    :when (node-filter dst)]
              (let [cost-from-node-to-dst (cost-fn edge),
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

(defn- least-cost-path-seq-helper
  "Variation that produces a seq of paths produced during the traversal"
  [g goal? ^PriorityQueue queue ^HashMap least-costs 
   ^HashMap backlinks cost-fn node-filter edge-filter]
  (let [stepfn 
        (fn stepfn []
          (println "stepfn")
          (loop []
            (when-let [[cost-from-start-to-node node] (.poll queue)]
              (cond 
                (goal? node) [(->Path (delay (find-path node backlinks)) (.get least-costs node) node)]
                (> cost-from-start-to-node (.get least-costs node)) (recur)
                :else
                (cons
                  (->Path (delay (find-path node backlinks)) (.get least-costs node) node)
                  (lazy-seq
                    (do (doseq [edge (uber/out-edges g node)
                                :when (edge-filter edge)
                                :let [dst (uber/dest edge)]
                                :when (node-filter dst)]
                          (let [cost-from-node-to-dst (cost-fn edge),
                                cost-from-start-to-dst (+ cost-from-start-to-node 
                                                          cost-from-node-to-dst)
                                least-cost-found-so-far-from-start-to-dst (.get least-costs dst)]
                            (when (or (not least-cost-found-so-far-from-start-to-dst) 
                                      (< cost-from-start-to-dst least-cost-found-so-far-from-start-to-dst))
                              (.add queue [cost-from-start-to-dst dst])
                              (.put least-costs dst cost-from-start-to-dst)
                              (.put backlinks dst edge))))
                     (stepfn))))))))]
    (stepfn)))

(defn- least-cost-path
  "Takes a graph g, a collection of starting nodes, a goal? predicate, and optionally a cost function
(defaults to weight). Returns a list of edges that form a path with the least cost 
from one of the starting nodes to a node that satisfies the goal? predicate."  
  [g starting-nodes goal? cost-fn node-filter edge-filter traverse?]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [0 node]))
    (if traverse?
      (least-cost-path-seq-helper g goal? queue least-costs backlinks cost-fn node-filter edge-filter)
      (least-cost-path-helper g goal? queue least-costs backlinks cost-fn node-filter edge-filter))))

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

(defn- least-cost-path-with-heuristic-seq-helper
  "Variation that produces seq of paths traversed"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn node-filter edge-filter]
  (let [stepfn
        (fn stepfn []
          (when-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
            (cond
              (goal? node) [(->Path (find-path node backlinks) (.get least-costs node))]
              (> cost-from-start-to-node (.get least-costs node)) (recur)
              :else
              (cons (->Path (find-path node backlinks) (.get least-costs node))
                    (lazy-seq 
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
                        (stepfn)))))))]
    (stepfn)))
        
(defn- least-cost-path-with-heuristic
  "Heuristic function must take a single node as an input, and return
   a lower bound of the cost from that node to a goal node"
  [g starting-nodes goal? cost-fn heuristic-fn node-filter edge-filter traverse?]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [(heuristic-fn node) [0 node]]))
    (if traverse?
      (least-cost-path-with-heuristic-seq-helper g goal? queue least-costs cost-fn heuristic-fn node-filter edge-filter)
      (least-cost-path-with-heuristic-helper g goal? queue least-costs cost-fn heuristic-fn node-filter edge-filter))))

(defn shortest-path 
  "Finds the shortest path in graph g. You must specify a start node or a collection
of start nodes from which to begin the search, however specifying an end node
is optional. If an end node condition is specified, this function will return an 
implementation of the IPath protocol, representing the shortest path. Otherwise, 
it will search out as far as it can go, and return an implementation of the 
IAllPathsFromSource protocol, which contains all the data needed to quickly find
the shortest path to a given destination (using IAllPathsFromSource's `path-to` 
protocol function).

If :traverse is set to true, then the function will instead return a lazy sequence
of the shortest paths from the start node(s) to each node in the graph in the order
the nodes are encountered by the search process.

Takes a map which must contain:
Either :start-node (single node) or :start-nodes (collection)

Map may contain the following entries:
Either :end-node (single node) or :end-nodes (collection) or :end-node? (predicate function) 
:cost-fn - A function that takes an edge as an input and returns a cost 
          (defaults to every edge having a cost of 1, i.e., breadth-first search if no cost-fn given)
:cost-attr - Alternatively, can specify an edge attribute to use as the cost
:heuristic-fn - A function that takes a node as an input and returns a
          lower-bound on the distance to a goal node, used to guide the search
          and make it more efficient.
:node-filter - A predicate function that takes a node and returns true or false.
          If specified, only nodes that pass this node-filter test will be considered in the search.
:edge-filter - A predicate function that takes an edge and returns true or false.
          If specified, only edges that pass this edge-filter test will be considered in the search.

Map may contain the following additional entries if a traversal sequence is desired:
:traverse true - Changes output to be a sequence of paths in order encountered.
:min-cost - Filters traversal sequence, only applies if :traverse is set to true
:max-cost - Filters traversal sequence, only applies if :traverse is set to true
"
  ([g search-specification]
    (assert (not (and (get search-specification :start-node)
                      (get search-specification :start-nodes)))
            "Can't specify both :start-node and :start-nodes")
    (assert (<= 2 (count (filter nil? (map search-specification [:end-node :end-nodes :end-node?]))))
            "Pick only one of :end-node, :end-nodes, or :end-node?")
    (assert (not (and (get search-specification :cost-fn) 
                      (get search-specification :cost-attr))) 
            "Can't specify both a :cost-fn and a :cost-attr")
    (let [cost-attr (get search-specification :cost-attr)
          cost-fn (if cost-attr
                    #(uber/attr g % cost-attr)
                    (get search-specification :cost-fn))
          heuristic-fn (get search-specification :heuristic-fn)
          node-filter (get search-specification :node-filter (constantly true))
          edge-filter (get search-specification :edge-filter (constantly true))
          starting-nodes (if-let [start-node (:start-node search-specification)]
                           [start-node]
                           (:start-nodes search-specification))
          traversal? (:traverse search-specification)
          goal? (cond
                  (:end-node search-specification) #{(:end-node search-specification)}
                  (:end-nodes search-specification) (set (:end-nodes search-specification))
                  (:end-node? search-specification) (:end-node? search-specification)
                  :else no-goal)
          min-cost (get search-specification :min-cost 0)
          max-cost (get search-specification :max-cost java.lang.Double/POSITIVE_INFINITY)]
      (assert (<= min-cost max-cost) ":min-cost must be less-than-or-equal to :max-cost")
      (assert (or (not (or (:min-cost search-specification) (:max-cost search-specification)))
                  traversal?)
              ":min-cost and :max-cost have no effect unless you set :traverse to true")
             
      (cond
        (and (nil? cost-fn) (nil? cost-attr) (nil? heuristic-fn))
        (least-edges-path g starting-nodes goal? node-filter edge-filter traversal?),
        
        heuristic-fn
        (least-cost-path-with-heuristic
          g starting-nodes goal? (if cost-fn cost-fn (constantly 1)) heuristic-fn node-filter edge-filter traversal?),
        
        :else
        (least-cost-path g starting-nodes goal? cost-fn node-filter edge-filter traversal?)))))
  
