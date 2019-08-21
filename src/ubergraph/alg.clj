(ns ubergraph.alg
  "Contains algorithms that operate on Ubergraphs, and all the functions associated with paths"
  (:require [ubergraph.core :as uber]
            [ubergraph.protocols :as prots]
            [potemkin :refer [import-vars]]
            [clojure.core.reducers :as r]
            [loom.graph :as lg]
            [loom.attr :as la]
            loom.alg)
  (:import java.util.PriorityQueue java.util.HashMap java.util.LinkedList java.util.HashSet
           java.util.Collections java.util.Map))

;; Various searches for shortest paths
;; For speed, on Java, use mutable queues and hash maps
;; Consider using Clojure data structures for better portability to Clojurescript

(import-vars
 [ubergraph.protocols
  ;; Path protocols
  edges-in-path
  nodes-in-path
  cost-of-path
  start-of-path
  end-of-path
  last-edge-of-path
  path-to
  all-destinations]
 ;;   path-between  Reserved for future use in all-paths algorithms
 
 [loom.alg
  ;; Curated loom algorithms
  connected-components
  connected?
  pre-traverse
  pre-span
  post-traverse
  topsort
  bf-traverse
  bf-span
  dag?
  scc
  strongly-connected?
  connect
  coloring?
  greedy-coloring
  degeneracy-ordering
  maximal-cliques])

;; TBD - Looking for volunteer to analyze flow and minimum spanning tree algos
;;       from loom and ensure correctness for graphs with parallel edges.

(declare find-path)

(defrecord Path [list-of-edges cost end last-edge]
  ubergraph.protocols/IPath
  (edges-in-path [this] @list-of-edges)
  (nodes-in-path [this] (when (seq @list-of-edges)
                          (cons (uber/src (first @list-of-edges))
                                (map uber/dest @list-of-edges))))
  (cost-of-path [this] cost)
  (end-of-path [this] end)
  (start-of-path [this] (first (nodes-in-path this)))
  (last-edge-of-path [this] (when (not (identical? last-edge ())) last-edge)))

(defrecord AllPathsFromSource [^Map backlinks ^Map least-costs]
  ubergraph.protocols/IAllPathsFromSource
  (path-to [this dest]
    (when-let [last-edge (.get backlinks dest)]
      (->Path (delay (find-path dest backlinks))
              (.get least-costs dest)
              dest
              last-edge)))
  (all-destinations [this]
    (keys backlinks)))

(defrecord AllBFSPathsFromSource [^Map backlinks ^Map depths]
  ubergraph.protocols/IAllPathsFromSource
  (path-to [this dest]
    (when-let [last-edge (.get backlinks dest)]
      (->Path (delay (find-path dest backlinks))
              (.get depths dest)
              dest
              last-edge)))
  (all-destinations [this]
    (keys backlinks)))

(alter-meta! #'->Path assoc :no-doc true)
(alter-meta! #'->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'->AllBFSPathsFromSource assoc :no-doc true)
(alter-meta! #'map->Path assoc :no-doc true)
(alter-meta! #'map->AllPathsFromSource assoc :no-doc true)
(alter-meta! #'map->AllBFSPathsFromSource assoc :no-doc true)


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

(defn- edge-attrs "Figure out edge attributes if no graph specified"
  [edge]
  (cond (vector? edge) (nth edge 2)
        (meta edge) (uber/attrs (meta edge) edge)
        :else {}))

(defn pprint-path 
  "Prints a path's edges along with the edges' attribute maps. 
  (pprint-path g p) will print the attribute maps currently stored in graph g for each edge in p.
  (pprint-path p) will print the attribute maps associated with each edge in p at the time the path was generated."   
  ([p]
   (println "Total Cost:" (cost-of-path p))
   (doseq [edge (edges-in-path p)]      
     (println (uber/src edge) "->" (uber/dest edge)
              (let [a (edge-attrs edge)]
                (if (seq a) a "")))))
  ([g p]
   (println "Total Cost:" (cost-of-path p))
   (doseq [edge (edges-in-path p)]      
     (println (uber/src edge) "->" (uber/dest edge)
              (let [a (uber/attrs g edge)]
                (if (seq a) a ""))))))


(defn- find-path
  "Work backwards from the destination to reconstruct the path"
  ([to backlinks] (find-path to backlinks ()))
  ([to ^Map backlinks path]
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
          (->Path (delay (find-path node backlinks)) depth node
                  (.get backlinks node))
          (do
            (doseq [edge (uber/out-edges g node)
                    :when (edge-filter edge)]
              (let [dst (uber/dest edge)
                    inc-depth (inc depth)]
                (when (and (node-filter dst) (not (.get backlinks dst)))
                  (.add queue dst)
                  (.put depths dst inc-depth)
                  (.put backlinks dst edge))))
            (recur))))
      (if (identical? no-goal goal?)
        (->AllBFSPathsFromSource (Collections/unmodifiableMap backlinks)
                                 (Collections/unmodifiableMap depths))
        nil))))

(defn- least-edges-path-seq-helper
  "Variation that produces a seq of paths produced during the traversal"
  [g goal? ^LinkedList queue ^HashMap backlinks ^HashMap depths node-filter edge-filter min-cost max-cost]
  (let [explore-node (fn [node depth]
                       (doseq [edge (uber/out-edges g node)
                               :when (edge-filter edge)]
                         (let [dst (uber/dest edge)
                               inc-depth (inc depth)]
                           (when (and (node-filter dst) (not (.get backlinks dst)))
                             (.add queue dst)
                             (.put depths dst inc-depth)
                             (.put backlinks dst edge)))))
        stepfn 
        (fn stepfn [] 
          (when-let [node (.poll queue)]
            (let [depth (.get depths node)]
              (if (<= min-cost depth max-cost)
                (cons
                 (->Path (delay (find-path node backlinks)) depth node
                         (.get backlinks node))
                 (lazy-seq
                  (if (goal? node)
                    nil
                    (do
                      (explore-node node depth)
                      (stepfn)))))
                (if (goal? node)
                  nil
                  (do
                    (explore-node node depth)
                    (recur)))))))]

    (stepfn)))

(defn- least-edges-path
  "Takes a graph g, a collection of starting nodes, and a goal? predicate. Returns
  a path that gets you from one of the starting nodes to a node that satisfies the goal? predicate 
  using the fewest possible edges."
  [g starting-nodes goal? node-filter edge-filter traverse? min-cost max-cost]
  (let [queue (LinkedList.),
        backlinks (HashMap.)
        depths (HashMap.)]
    (doseq [node starting-nodes :when (and (uber/has-node? g node)
                                           (node-filter node))]
      (.add queue node)
      (.put depths node 0)
      (.put backlinks node ()))
    (if traverse?
      (least-edges-path-seq-helper g goal? queue backlinks depths node-filter edge-filter min-cost max-cost)
      (least-edges-path-helper g goal? queue backlinks depths node-filter edge-filter))))

(defn- least-cost-path-helper
  "Find the shortest path with respect to the cost-fn applied to edges"
  [g goal? ^PriorityQueue queue ^HashMap least-costs 
   ^HashMap backlinks cost-fn node-filter edge-filter]
  (loop []
    (if-let [[cost-from-start-to-node node] (.poll queue)]
      (cond 
        (goal? node) (->Path (delay (find-path node backlinks))
                             (.get least-costs node) node
                             (.get backlinks node))
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
        (->AllPathsFromSource (Collections/unmodifiableMap backlinks)
                              (Collections/unmodifiableMap least-costs))
        nil))))

(defn- least-cost-path-seq-helper
  "Variation that produces a seq of paths produced during the traversal"
  [g goal? ^PriorityQueue queue ^HashMap least-costs 
   ^HashMap backlinks cost-fn node-filter edge-filter min-cost max-cost]
  (let [explore-node 
        (fn [node cost-from-start-to-node]
          (doseq [edge (uber/out-edges g node)
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
                (.put backlinks dst edge))))),
        stepfn 
        (fn stepfn []
          (loop []
            (when-let [[cost-from-start-to-node node] (.poll queue)]
              (cond 
                (or (< cost-from-start-to-node min-cost)
                    (< max-cost cost-from-start-to-node))
                (do (explore-node node cost-from-start-to-node) (recur)),
                
                (goal? node)
                [(->Path (delay (find-path node backlinks))
                         (.get least-costs node) node
                         (.get backlinks node))],
                (> cost-from-start-to-node (.get least-costs node)) (recur)
                :else
                (cons
                 (->Path (delay (find-path node backlinks))
                         (.get least-costs node) node
                         (.get backlinks node))
                 (lazy-seq
                  (do (explore-node node cost-from-start-to-node) (stepfn))))))))]
    (stepfn)))

(defn- least-cost-path
  "Takes a graph g, a collection of starting nodes, a goal? predicate, and optionally a cost function
  (defaults to weight). Returns a list of edges that form a path with the least cost 
  from one of the starting nodes to a node that satisfies the goal? predicate."  
  [g starting-nodes goal? cost-fn node-filter edge-filter traverse? min-cost max-cost]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue. (fn [x y] (compare (x 0) (y 0))))]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [0 node]))
    (if traverse?
      (least-cost-path-seq-helper g goal? queue least-costs backlinks cost-fn node-filter edge-filter min-cost max-cost)
      (least-cost-path-helper g goal? queue least-costs backlinks cost-fn node-filter edge-filter))))

(defn- least-cost-path-with-heuristic-helper
  "AKA A* search"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn node-filter edge-filter]
  (loop []
    (if-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
      (cond
        (goal? node) (->Path (delay (find-path node backlinks))
                             (.get least-costs node) node
                             (.get backlinks node))
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
        (->AllPathsFromSource (Collections/unmodifiableMap backlinks) (Collections/unmodifiableMap least-costs))
        nil))))

(defn- least-cost-path-with-heuristic-seq-helper
  "Variation that produces seq of paths traversed"
  [g goal? ^PriorityQueue queue ^HashMap least-costs ^HashMap backlinks cost-fn heuristic-fn node-filter edge-filter min-cost max-cost]
  (let [explore-node 
        (fn [node cost-from-start-to-node]
          (doseq [edge (uber/out-edges g node)
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
                (.put backlinks dst edge))))),
        stepfn
        (fn stepfn []
          (when-let [[estimated-total-cost-through-node [cost-from-start-to-node node]] (.poll queue)]
            (cond
              (or (< cost-from-start-to-node min-cost)
                  (< max-cost cost-from-start-to-node))
              (do (explore-node node cost-from-start-to-node) (recur)),

              (goal? node)
              [(->Path (delay (find-path node backlinks)) 
                       (.get least-costs node) node
                       (.get backlinks node))]
              (> cost-from-start-to-node (.get least-costs node)) (recur)
              :else
              (cons (->Path (delay (find-path node backlinks))
                            (.get least-costs node) node
                            (.get backlinks node))
                    (lazy-seq 
                     (do (explore-node node cost-from-start-to-node) (stepfn)))))))]
    (stepfn)))

(defn- least-cost-path-with-heuristic
  "Heuristic function must take a single node as an input, and return
   a lower bound of the cost from that node to a goal node"
  [g starting-nodes goal? cost-fn heuristic-fn node-filter edge-filter traverse? min-cost max-cost]
  (let [least-costs (HashMap.),
        backlinks (HashMap.)
        queue (PriorityQueue. (fn [x y] (compare (x 0) (y 0))))]
    (doseq [node starting-nodes :when (and (uber/has-node? g node) (node-filter node))]
      (.put least-costs node 0)
      (.put backlinks node ())
      (.add queue [(heuristic-fn node) [0 node]]))
    (if traverse?
      (least-cost-path-with-heuristic-seq-helper g goal? queue least-costs backlinks cost-fn heuristic-fn node-filter edge-filter min-cost max-cost)
      (least-cost-path-with-heuristic-helper g goal? queue least-costs backlinks cost-fn heuristic-fn node-filter edge-filter))))

(declare bellman-ford)

(def ^:dynamic ^{:doc "Bind this dynamic variable to false if you prefer for shortest-path to throw an error, if negative cost edge is found."}
  *auto-bellman-ford* true) 

(defn- out-edges-fn->graph
  "Implements the protocols necessary to do a search"
  [out-edges-fn]
  (reify
    lg/Graph
    (has-node? [g node] true)
    (out-edges [g node] (for [{:keys [dest] :as edge} (out-edges-fn node)]
                          [node dest (dissoc edge :dest)]))
    la/AttrGraph
    (attrs [g edge]
      (let [attrs (nth edge 2)]
        (cond (map? attrs) attrs
              (number? attrs) {:weight attrs}
              :else {:weight 1})))
    (attr [g edge k]
      (get (la/attrs g edge) k))))

(defn shortest-path 
  "Finds the shortest path in g, where g is either an ubergraph or a
  transition function that implies a graph. A transition function 
  takes the form: (fn [node] [{:dest successor1, ...} {:dest successor2, ...} ...])

  You must specify a start node or a collection
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

  Takes a search-specification map which must contain:
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


  shortest-path has specific arities for the two most common combinations:
  (shortest-path g start-node end-node)
  (shortest-path g start-node end-node cost-attr)
  "
  ([g start-node end-node] (shortest-path g {:start-node start-node, :end-node end-node}))
  ([g start-node end-node cost-attr] (shortest-path g {:start-node start-node, :end-node end-node, :cost-attr cost-attr}))
  ([g search-specification]
   (assert (map? search-specification) "Second input must be a map, see docstring for options")
   (assert (not (and (get search-specification :start-node)
                     (get search-specification :start-nodes)))
           "Can't specify both :start-node and :start-nodes")
   (assert (<= 2 (count (filter nil? (map search-specification [:end-node :end-nodes :end-node?]))))
           "Pick only one of :end-node, :end-nodes, or :end-node?")
   (assert (not (and (get search-specification :cost-fn) 
                     (get search-specification :cost-attr))) 
           "Can't specify both a :cost-fn and a :cost-attr")
   (let [g (if-not (fn? g) g
                   (out-edges-fn->graph g))
         cost-attr (get search-specification :cost-attr)
         cost-fn (if cost-attr
                   #(uber/attr g % cost-attr)
                   (get search-specification :cost-fn))
         cost-fn (when cost-fn
                   (fn [edge]
                     (let [cost (cost-fn edge)]
                       (if (neg? cost)
                         (throw (IllegalStateException. "Negative edge, retry with Bellman-Ford alg"))
                         cost))))
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
         min-cost (get search-specification :min-cost java.lang.Double/NEGATIVE_INFINITY)
         max-cost (get search-specification :max-cost java.lang.Double/POSITIVE_INFINITY)]
     (assert (<= min-cost max-cost) ":min-cost must be less-than-or-equal to :max-cost")
     (assert (or (not (or (:min-cost search-specification) (:max-cost search-specification)))
                 traversal?)
             ":min-cost and :max-cost have no effect unless you set :traverse to true")

     (try
       (cond
         (and (nil? cost-fn) (nil? cost-attr) (nil? heuristic-fn))
         (least-edges-path g starting-nodes goal? node-filter edge-filter traversal? min-cost max-cost),
         
         heuristic-fn
         (least-cost-path-with-heuristic
          g starting-nodes goal? (if cost-fn cost-fn (constantly 1)) heuristic-fn node-filter edge-filter traversal? min-cost max-cost),
         
         :else
         (least-cost-path g starting-nodes goal? cost-fn node-filter edge-filter traversal? min-cost max-cost))
       (catch IllegalStateException e 
         (if *auto-bellman-ford*
           (bellman-ford g search-specification)
           (throw (IllegalStateException. "Found edge with negative cost. Use bellman-ford."))))))))

(defn paths->graph "Takes output of shortest-path and returns the graph of directed edges implied by the search process"
  [paths]
  (cond
    (satisfies? ubergraph.protocols/IAllPathsFromSource paths)
    (let [^Map backlinks (:backlinks paths)]
      (apply uber/digraph
             (for [[node edge] (seq backlinks)
                   init (if (= edge ())
                          [[node {:cost-of-path (cost-of-path (path-to paths node))}]]
                          [[node {:cost-of-path (cost-of-path (path-to paths node))}]
                           ^:edge [(uber/src (.get backlinks node))
                                   node (edge-attrs edge)]])]
               init)))

    (satisfies? ubergraph.protocols/IPath paths)
    (apply uber/digraph [(end-of-path paths) {:cost-of-path (cost-of-path paths)}]
           (for [edge (edges-in-path paths)]
             ^:edge [(uber/src edge) (uber/dest edge) (edge-attrs edge)]))

    :else
    (apply uber/digraph
           (for [path paths
                 :let [edge (last-edge-of-path path)]
                 init (if-not edge
                        [[(end-of-path path) {:cost-of-path (cost-of-path path)}]]
                        [[(end-of-path path) {:cost-of-path (cost-of-path path)}]
                         ^:edge [(uber/src edge) (uber/dest edge) (edge-attrs edge)]])]
             init))))

;; Algorithms similar to those in Loom, adapted for Ubergraphs

(defn loners
  "Return nodes with no connections to other nodes (i.e., isolated nodes)"
  [g]
  (for [node (uber/nodes g)
        :when (and (zero? (uber/in-degree g node))
                   (zero? (uber/out-degree g node)))]
    node))

(defn distinct-edges
  "Distinct edges of g."
  [g]
  (if (uber/ubergraph? g)
    (for [edge (uber/edges g)
          :when (not (uber/mirror-edge? edge))]
      edge)
    (loom.alg/distinct-edges g)))

(defn longest-shortest-path
  "The longest shortest-path starting from start"
  [g start]
  (last (shortest-path g {:start-node start,
                          :traverse true})))

;; Bellman-Ford, adapted from Loom

(defn- can-relax-edge?
  "Test for whether we can improve the shortest path to v found so far
   by going through u."
  [[u v :as edge] cost costs]
  (let [vd (get costs v)
        ud (get costs u)
        sum (+ ud cost)]
    (> vd sum)))

(defn- relax-edge
  "If there's a shorter path from s to v via u,
    update our map of estimated path costs and
   map of paths from source to vertex v"
  [[u v :as edge] cost [costs backlinks :as estimates]]
  (let [ud (get costs u)
        sum (+ ud cost)]
    (if (can-relax-edge? edge cost costs)
      [(assoc costs v sum) (assoc backlinks v edge)]
      estimates)))

(defn- relax-edges
  "Performs edge relaxation on all edges in weighted directed graph"
  [edges estimates cost-fn]
  (let [new-estimates
        (->> (edges)
             (reduce (fn [estimates edge]
                       (relax-edge edge (cost-fn edge) estimates))
                     estimates))]
    (if (identical? estimates new-estimates)
                                        ; If no edge relaxed in this pass, we know for sure we're done
      (reduced (with-meta new-estimates {:bellman-ford-complete true}))
      new-estimates)))

(defn- init-estimates
  "Initializes path cost estimates and paths from source to all vertices,
   for Bellman-Ford algorithm"
  [graph starting-nodes node-filter]
  (let [starting-node-set (set starting-nodes)
        nodes (for [node (uber/nodes graph) 
                    :when (and (node-filter node)
                               (not (starting-node-set node)))] 
                node)
        path-costs (into {} (for [node starting-nodes] [node 0]))
        backlinks (into {} (for [node starting-nodes] [node ()]))
        infinities (repeat Double/POSITIVE_INFINITY)
        nils (repeat ())
        init-costs (interleave nodes infinities)
        init-backlinks (interleave nodes nils)]
    [(apply assoc path-costs init-costs)
     (apply assoc backlinks init-backlinks)]))

(defn bellman-ford
  "Given an ubergraph g, and one or more start nodes,
  the Bellman-Ford algorithm produces an implementation of the
  IAllPathsFromSource protocol if no negative-weight cycle that is 
  reachable from the source exits, and false otherwise, indicating 
  that no solution exists.

  bellman-ford is very similar to shortest-path.  It is less efficient,
  but it correctly handles graphs with negative edges.  If you know you
  have edges with negative costs, use bellman-ford.  If you are unsure
  whether your graph has negative costs, or don't understand when and
  why you'd want to use bellman-ford, just use shortest-path and it
  will make the decision for you, calling this function if necessary. 

  Takes a search-specification map which must contain:
  Either :start-node (single node) or :start-nodes (collection)

  Map may contain the following entries:
  Either :end-node (single node) or :end-nodes (collection) or :end-node? (predicate function) 
  :cost-fn - A function that takes an edge as an input and returns a cost 
          (defaults to weight, or 1 if no weight is present)
  :cost-attr - Alternatively, can specify an edge attribute to use as the cost
  :node-filter - A predicate function that takes a node and returns true or false.
          If specified, only nodes that pass this node-filter test will be considered in the search.
  :edge-filter - A predicate function that takes an edge and returns true or false.
          If specified, only edges that pass this edge-filter test will be considered in the search.

  Map may contain the following additional entries if a traversal sequence is desired:
  :traverse true - Changes output to be a sequence of paths in order encountered.
  :min-cost - Filters traversal sequence, only applies if :traverse is set to true
  :max-cost - Filters traversal sequence, only applies if :traverse is set to true

  bellman-ford has specific arity for the most common combination:
  (bellman-ford g start-node cost-attr)
  "
  ([g start-node cost-attr] (bellman-ford g {:start-node start-node :cost-attr cost-attr}))
  ([g search-specification]
   (assert (map? search-specification) "Second input must be a map, see docstring for options")
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
         node-filter (get search-specification :node-filter (constantly true))
         edge-filter (get search-specification :edge-filter (constantly true))
         starting-nodes (if-let [start-node (:start-node search-specification)]
                          [start-node]
                          (:start-nodes search-specification))
         starting-nodes (filter #(and (uber/has-node? g %)
                                      (node-filter %))
                                starting-nodes)
         valid-nodes (filter node-filter (uber/nodes g))
         end-nodes (cond
                     (:end-node search-specification) [(:end-node search-specification)]
                     (:end-nodes search-specification) (:end-nodes search-specification)
                     (:end-node? search-specification) (filter (:end-node? search-specification) valid-nodes)
                     :else nil)
         goal? (set end-nodes)          
         traversal? (:traverse search-specification)
         min-cost (get search-specification :min-cost java.lang.Double/NEGATIVE_INFINITY)
         max-cost (get search-specification :max-cost java.lang.Double/POSITIVE_INFINITY)]
     (assert (<= min-cost max-cost) ":min-cost must be less-than-or-equal to :max-cost")
     (assert (or (not (or (:min-cost search-specification) (:max-cost search-specification)))
                 traversal?)
             ":min-cost and :max-cost have no effect unless you set :traverse to true")

     (when (seq starting-nodes)
       (let [initial-estimates (init-estimates g starting-nodes node-filter)
             edges (fn [] (for [n (shuffle valid-nodes)  ;shuffling nodes improves running time
                                :when (node-filter n)
                                e (uber/out-edges g n)
                                :when (and (edge-filter e) (node-filter (uber/dest e)))]
                            e))
             ;;relax-edges is calculated for all edges V-1 times
             [costs backlinks :as answer] (reduce (fn [estimates _]
                                                    (relax-edges edges estimates cost-fn))
                                                  initial-estimates
                                                  (range (dec (count valid-nodes))))]
         (if (and (not (:bellman-ford-complete (meta answer)))
                  (some
                   (fn [edge] (can-relax-edge? edge (cost-fn edge) costs))
                   (edges)))
           false
           (let [backlinks (reduce (fn [links node] (if (= Double/POSITIVE_INFINITY (get costs node))
                                                      (dissoc links node)
                                                      links))
                                   backlinks
                                   valid-nodes)
                 all-paths-from-source (->AllPathsFromSource (Collections/unmodifiableMap backlinks) (Collections/unmodifiableMap costs))]
             (cond
               traversal?
               (->> (vec valid-nodes)
                    (r/map #(path-to all-paths-from-source %))
                    (r/filter #(<= min-cost (cost-of-path %) max-cost))
                    r/foldcat
                    sort),
               
               end-nodes
               (->> (vec end-nodes)
                    (r/map #(path-to all-paths-from-source %))
                    r/foldcat
                    (apply min-key cost-of-path))
               
               :else
               all-paths-from-source))))))))


;; Some things from Loom that don't quite work as-is

;; Loom's bipartite has a bug where it only calls successors to get neighbors,
;; which doesn't work for directed edges.
;; https://github.com/aysylu/loom/issues/118

(defn bipartite-color
  "Attempts a two-coloring of graph g. When successful, returns a map of
  nodes to colors (1 or 0). Otherwise, returns nil."
  [g]
  (letfn [(color-component [coloring start]
            (loop [coloring (assoc coloring start 1)
                   queue (conj clojure.lang.PersistentQueue/EMPTY start)]
              (if (empty? queue)
                coloring
                (let [v (peek queue)
                      color (- 1 (coloring v))
                      nbrs (uber/neighbors g v)]
                  (if (some #(and (coloring %) (= (coloring v) (coloring %)))
                            nbrs)
                    nil ; graph is not bipartite
                    (let [nbrs (remove coloring nbrs)]
                      (recur (into coloring (for [nbr nbrs] [nbr color]))
                             (into (pop queue) nbrs))))))))]
    (loop [[node & nodes] (seq (uber/nodes g))
           coloring {}]
      (when coloring
        (if (nil? node)
          coloring
          (if (coloring node)
            (recur nodes coloring)
            (recur nodes (color-component coloring node))))))))

(defn bipartite?
  "Returns true if g is bipartite"
  [g]
  (boolean (bipartite-color g)))

(defn bipartite-sets
  "Returns two sets of nodes, one for each color of the bipartite coloring,
  or nil if g is not bipartite"
  [g]
  (when-let [coloring (bipartite-color g)]
    (reduce
     (fn [[s1 s2] [node color]]
       (if (zero? color)
         [(conj s1 node) s2]
         [s1 (conj s2 node)]))
     [#{} #{}]
     coloring)))
