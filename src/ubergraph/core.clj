(ns ubergraph.core
  (:require [potemkin :refer [import-vars def-map-type deftype+]]
            [potemkin.collections :refer [AbstractMap]]
            [com.rpl.specter :refer [select-any selected-any? select
                                     transform multi-transform if-path
                                     keypath must compact multi-path
                                     nil->val terminal setval
                                     STOP ALL NONE MAP-VALS]]
            [loom.graph :as lg]
            [loom.attr :as la]
            [ubergraph.protocols :as up]
            [dorothy.core :as d]
            [dorothy.jvm :as dj]
            [clojure.string :as str]
            [clojure.pprint]))

(import-vars
 [loom.graph
  ;; Graph protocol
  nodes
  edges
  has-node?
  has-edge?
  successors*
  out-degree
  out-edges
  ;; Digraph protocol
  predecessors*
  in-degree
  in-edges
  transpose
  ;; WeightedGraph protocol
  weight*
  ;; EditableGraph protocol
  add-nodes*
  add-edges*
  remove-nodes*
  remove-edges*
  remove-all
  ;; Edge protocol
  src
  dest
  ;; Helper functions
  successors
  predecessors
  add-nodes
  add-edges
  remove-nodes
  remove-edges
  subgraph]

 [loom.attr
  ;; AttrGraph protocol
  add-attr
  remove-attr
  attr
  attrs]

 [ubergraph.protocols
  ;; UndirectedGraph protocol
  other-direction
  ;; QueryableGraph protocol
  find-edges
  find-edge
  ;; Attrs protocol
  add-attrs
  remove-attrs
  set-attrs
  ;; MixedDirectionEdgeTests protocol
  undirected-edge?
  directed-edge?
  mirror-edge?
  ;; MixedDirectionGraph protocol
  add-directed-edges*
  add-undirected-edges*
  ;; Ubergraph recognition protocol
  ubergraph?])
   
   

;; We extend add-edges to support attribute maps in the edge specification
;; so let's update the doc string
(alter-meta! #'add-edges assoc
             :doc "Adds edges to graph g of the form [n1 n2], [n1 n2 weight], or [n1 n2 attr-map].")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make many doc strings more informative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alter-meta!
 #'nodes assoc :doc
 "Return a sequence of the nodes in graph g.

  For the implementation on type Ubergraph, returns in O(1) time, with
  effectively O(n) time to traverse all n nodes.")

(alter-meta!
 #'edges assoc :doc
 "Return a sequence of the edges of graph g.  Any undirected edges in
  the graph appear twice in this sequence, one as an edge e1
  with (mirror-edge? e1) equal to true, the other as an edge e2
  with (mirror-edge? e2) equal to false.  This is so even for
  undirected self loop edges, i.e. with both node endpoints equal to
  each other.

  For the implementation on type Ubergraph, returns in O(1) time, with
  effectively O(m) time to traverse all m edges.")

(alter-meta!
 #'has-node? assoc :doc
 "Return true if node is in g, otherwise false.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time, the same as looking up a value as a key in a map.")

(alter-meta!
 #'has-edge? assoc :doc
 "Return true if there is an edge from node n1 to n2 in g, otherwise
  false.  There is an edge from node n1 to n2 if there is a directed
  edge from n1 to n2, or if there is an undirected edge between those
  two nodes.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time.")

(alter-meta!
 #'out-degree assoc :doc
 "Return the number of outgoing edges of node.  This is the same value
  as (count (ubergraph.core/out-edges g node)).

  For the implementation on type Ubergraph, returns the answer from a
  precalculated stored value, in effectively O(1) time.")

(alter-meta!
 #'out-edges assoc :doc
 "Return a sequence of all of the outgoing edges of node.  If the
  graph has multiple parallel edges between nodes, all of them are
  included.

  If the node has any directed 'self loop' edges to itself, they are
  included once.  Undirected self loop edges are included twice, one
  with mirror-edge? equal to true, the other with mirror-edge? equal
  to false.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time, with effectively O(k) time to traverse the sequence of k
  edges.")

(alter-meta!
 #'in-degree assoc :doc
 "Return the number of incoming edges of node.  This is the same value
  as (count (ubergraph.core/in-edges g node)).

  For the implementation on type Ubergraph, returns the answer from a
  precalculated stored value, in effectively O(1) time.")

(alter-meta!
 #'in-edges assoc :doc
 "Return a sequence of all of the incoming edges of node.  If the
  graph has multiple parallel edges between nodes, all of them are
  included.

  If the node has any directed 'self loop' edges to itself, they are
  included once.  Undirected self loop edges are included twice, one
  with mirror-edge? equal to true, the other with mirror-edge? equal
  to false.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time, with effectively O(k) time to traverse the sequence of k
  edges.")

(alter-meta!
 #'src assoc :doc
 "Returns the source node of the edge.

  For the implementation on type Ubergraph, returns in O(1) time.")

(alter-meta!
 #'dest assoc :doc
 "Returns the dest node of the edge.

  For the implementation on type Ubergraph, returns in O(1) time.")

(alter-meta!
 #'successors assoc :doc
  "Return a sequence of nodes in the graph g that are direct
  successors of the given node.  Node v is a direct successor of node
  u if there is a directed edge from u to v, or an undirected edge
  between them.  A node u is returned as a successor of itself if
  there is a 'self loop' edge from u to u.  Every successor node is
  guaranteed to appear exactly once in the returned sequence, even if
  there are multiple parallel edges that make it a successor.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time to return the sequence, with effectively O(k) time to
  traverse all k nodes returned.")

(alter-meta!
 #'predecessors assoc :doc
  "Return a sequence of nodes in the graph g that are direct
  predecessors of the given node.  Node v is a direct predecessor of
  node u if there is a directed edge from v to u, or an undirected
  edge between them.  A node u is returned as a predecessor of itself
  if there is a 'self loop' edge from u to u.  Every predecessor node
  is guaranteed to appear exactly once in the returned sequence, even
  if there are multiple parallel edges that make it a predecessor.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time to return the sequence, with effectively O(k) time to
  traverse all k nodes returned.")


;; This namespace provides a concrete implementation of ubergraph.protocols, which is
;; a conservative extension to Loom's protocols.

;; It supports undirected, directed, weighted, attributes, editing, mixed directedness,
;; and multiple edges between a given pair of vertices.

;; If Loom adopts the protocol extensions proposed in ubergraph.protocols, then
;; this data structure will be compatible with all loom algorithms.

;; At the bottom of this file, I demonstrate how one can use Ubergraph to implement
;; all the graph types provided by Loom (except Flygraph).  There are a couple
;; subtle implementation details that are different between the Ubergraph implementation
;; and the Loom default implementations of the protocols.

;; 1. Edge constructors (such as add-edges) support either [src dest], or [src dest weight]
;;    or [src dest attribute-map].
;;
;; 2. By default, edges added with the [src dest weight] constructor simply store the weight
;;    as a :weight attribute.  This is simply an implementation detail which can be completely
;;    ignored if you don't have any attributes and simply want to use the weight protocol
;;    to retrieve the weight of an edge.  But by making it an attribute, it has the
;;    added benefit that you can alter the weight of an edge using the attribute protocol.
;
;; 3. The edges that are returned by edges are not simple vectors, they are a custom Edge
;;    data structure.  All the functions that consume edges can take this custom Edge
;;    data structure, or simpler forms like [src dest] if that is enough to uniquely
;;    identify the edge.  It is recommended that edge-processing algorithms access the
;;    source and destination nodes using the new Edge protocol (src and dest), rather
;;    than assuming that an edge is a vector.
;;
;; 4. The build-graph semantics are somewhat different from Loom's. Since Ubergraphs
;;    are capable of holding both directed and undirected edges, if you build a
;;    directed graph from an undirected graph, those edges are imported as undirected,
;;    and conversely, if you build an undirected graph from a directed graph, those edges
;;    are imported as directed.


;; These are the functions that are too lengthy to define inline
;; in the Ubergraph record.
(declare transpose-impl get-edge find-edges-impl find-edge-impl add-node add-edge remove-node remove-edge
         edge-description->edge resolve-node-or-edge
         force-add-directed-edge force-add-undirected-edge remove-edges
         equal-graphs? hash-graph build-graph)

;; There is a bug in loom's implementation of weight. We fix it here.

(defn weight
  "Returns the weight of edge e or edge [n1 n2]"
  ([g] (partial weight g))
  ([g e] (weight* g e))
  ([g n1 n2] (weight* g n1 n2)))

(alter-var-root #'lg/weight (fn [x] weight))

(def-map-type Ubergraph [node-map allow-parallel? undirected? attrs cached-hash]
  AbstractMap
  lg/Graph
  (nodes [g] (keys (:node-map g)))
  (edges [g] (for [[node node-info] (:node-map g)
                   [dest edges] (:out-edges node-info),
                   edge edges]
               (with-meta edge g)))
  (has-node? [g node] (selected-any? (must :node-map node) g))
  (has-edge? [g n1 n2] (boolean (seq (find-edges g n1 n2))))
  (successors* [g node] (keys (select-any (keypath :node-map node :out-edges) g)))
  (out-degree [g node] (select-any (keypath :node-map node :out-degree) g))
  (out-edges [g node]
             (sequence (comp cat (map #(with-meta % g)))
                       (vals (select-any (keypath :node-map node :out-edges) g))))
  
  lg/Digraph
  (predecessors* [g node] (keys (select-any (keypath :node-map node :in-edges) g)))
  (in-degree [g node] (select-any (keypath :node-map node :in-degree) g))
  (in-edges [g node]
            (sequence (comp cat (map #(with-meta % g)))
                      (vals (select-any (keypath :node-map node :in-edges) g))))
  (transpose [g] (transpose-impl g))

  lg/WeightedGraph
  ;; Ubergraphs by default store weight in an attribute :weight
  ;; Using an attribute allows us to modify the weight with the AttrGraph protocol
  (weight* [g e]
           (select-any [(keypath :attrs (:id (edge-description->edge g e)) :weight)
                        (nil->val 1)] g))
  (weight* [g n1 n2]
           (select-any [(keypath :attrs (:id (get-edge g n1 n2)) :weight) (nil->val 1)]
                       g))

  lg/EditableGraph
  (add-nodes* [g nodes] (reduce add-node g nodes))
  ;; edge definition should be [src dest] or [src dest weight] or [src dest attribute-map]
  (add-edges* [g edge-definitions] (reduce (fn [g edge] (add-edge g edge)) g edge-definitions))
  (remove-nodes* [g nodes] (reduce remove-node g nodes))
  (remove-edges* [g edges] (reduce remove-edge g edges))
  (remove-all [g] (Ubergraph. {} allow-parallel? undirected? {} (atom -1)))

  la/AttrGraph
  (add-attr [g node-or-edge k v]
            (setval (keypath :attrs (resolve-node-or-edge g node-or-edge) k) v g))
  (add-attr [g n1 n2 k v] (add-attr g (get-edge g n1 n2) k v))
  (remove-attr [g node-or-edge k]
               (setval [(must :attrs (resolve-node-or-edge g node-or-edge))
                        (compact (keypath k))]
                       NONE g))
  (remove-attr [g n1 n2 k] (remove-attr g (get-edge g n1 n2) k))
  (attr [g node-or-edge k]
        (select-any (keypath :attrs (resolve-node-or-edge g node-or-edge) k) g))
  (attr [g n1 n2 k] (attr g (get-edge g n1 n2) k))
  (la/attrs [g node-or-edge]
            (select-any [(keypath :attrs (resolve-node-or-edge g node-or-edge))
                         (nil->val {})] g))
  (la/attrs [g n1 n2] (la/attrs g (get-edge g n1 n2)))

  up/Attrs
  (add-attrs [g node-or-edge attribute-map]
             (transform (keypath :attrs (resolve-node-or-edge g node-or-edge))
                        #(merge % attribute-map) g))
  (add-attrs [g n1 n2 attribute-map]             
             (add-attrs g (get-edge g n1 n2) attribute-map))
  (set-attrs [g node-or-edge attribute-map]
             (setval (keypath :attrs (resolve-node-or-edge g node-or-edge))
                     attribute-map g))
  (set-attrs [g n1 n2 attribute-map]
             (set-attrs g (get-edge g n1 n2) attribute-map))
  (remove-attrs [g node-or-edge attributes]
                (transform [(must :attrs (resolve-node-or-edge g node-or-edge))
                            (nil->val {})]
                           #(apply dissoc % attributes) g))
  (remove-attrs [g n1 n2 attributes]
                (remove-attrs g (get-edge g n1 n2) attributes))

  up/UndirectedGraph
  (other-direction [g edge]
                   (when (undirected-edge? edge)
                     (let [edge (edge-description->edge g edge),
                           e (assoc edge :src (:dest edge) :dest (:src edge) :mirror? (not (:mirror? edge)))]
                       e)))

  up/QueryableGraph
  (find-edges [g edge-query] (find-edges-impl g edge-query))
  (find-edges [g src dest] (find-edges-impl g src dest))
  (find-edge [g edge-query] (find-edge-impl g edge-query))
  (find-edge [g src dest] (find-edge-impl g src dest))

  up/MixedDirectionGraph
  (add-directed-edges* [g edge-definitions] (reduce (fn [g edge] (force-add-directed-edge g edge))
                                                    g edge-definitions))
  (add-undirected-edges* [g edge-definitions] (reduce (fn [g edge] (force-add-undirected-edge g edge))
                                                      g edge-definitions))

  up/IUbergraph
  (ubergraph? [g] true)

  (get [this key default-value]
       (case key
         :node-map node-map
         :allow-parallel? allow-parallel?
         :undirected? undirected?
         :attrs attrs
         :cached-hash cached-hash
         default-value))
  (assoc [this key value]
         (case key
           :node-map (Ubergraph. value allow-parallel? undirected? attrs cached-hash)
           :allow-parallel? (Ubergraph. node-map value undirected? attrs cached-hash)
           :undirected? (Ubergraph. node-map allow-parallel? value attrs cached-hash)
           :attrs (Ubergraph. node-map allow-parallel? undirected? value cached-hash)
           :cached-hash (Ubergraph. node-map allow-parallel? undirected? attrs value)
           this))
  (dissoc [this key] this)
  (keys [this] [:node-map :allow-parallel? :undirected? :attrs :cached-hash])
  (meta [this] nil)
  (with-meta [this meta] this)

  (hasheq [this] (hash-graph this))
  (equiv [this other] (and (instance? Ubergraph other)
                           (equal-graphs? this other)))
  )

(defn neighbors
  "Returns seq of nodes that are either successors or predecessors of the node"
  [g node]
  (distinct (concat (successors g node) (predecessors g node))))

(defn undirected-graph? "If true, new edges in g are undirected by default.  If false,
  new edges in g are directed by default."
  [g] (:undirected? g))

(defn allow-parallel-edges? "If true, two edges between the same pair of nodes in the same direction
  are permitted.  If false, adding a new edge between the same pair of nodes as an existing edge will
  merge the edges into a single edge, and adding an undirected edge on top of an existing directed edge
  will `upgrade' the directed edge to undirected and merge attributes."
  [g] (:allow-parallel? g))

;; A node-id is anything the user wants it to be -- a number, a keyword, a data structure
;; An edge is something with a src, a dest, and an id that can be used to look up attributes

;; node-map is a {node-id node-info}
;; node-info is a {:out-edges {dest-id #{edge}} :in-edges {src-id #{edge}}
;;                 :in-degree number :out-degree number}
;; edge is either Edge or UndirectedEdge

(defrecord NodeInfo [out-edges in-edges out-degree in-degree])
(defrecord Edge [id src dest]
  lg/Edge
  (src [edge] src)
  (dest [edge] dest)
  up/MixedDirectionEdgeTests
  (undirected-edge? [e] false)
  (directed-edge? [e] true)
  (mirror-edge? [e] false)
  clojure.lang.Indexed
  (nth [e i] (case i 0 src 1 dest 2 (attr (meta e) e :weight) nil))
  (nth [e i notFound] (case i 0 src 1 dest 2 (attr (meta e) e :weight) notFound)))

;; An UndirectedEdge stores an additional field that signals whether this was the
;; original direction that was added to the graph, or the "mirror" edge that was
;; automatically added to go in the reverse direction.  This is a useful concept
;; because in some undirected graph algorithms, you only want to consider each
;; edge once, so the mirror? field lets you filter out these duplicate reverse edges.

(defrecord UndirectedEdge [id src dest mirror?]
  lg/Edge
  (src [edge] src)
  (dest [edge] dest)
  up/MixedDirectionEdgeTests
  (undirected-edge? [e] true)
  (directed-edge? [e] false)
  (mirror-edge? [e] mirror?)
  clojure.lang.Indexed
  (nth [e i] (case i 0 src 1 dest 2 (attr (meta e) e :weight) nil))
  (nth [e i notFound] (case i 0 src 1 dest 2 (attr (meta e) e :weight) notFound)))

(extend-type
    Object
  up/MixedDirectionEdgeTests
  (undirected-edge? [e] false)
  (directed-edge? [e] false)
  (mirror-edge? [e] false)
  up/IUbergraph
  (ubergraph? [g] (and (satisfies? lg/Graph g)
                       (satisfies? lg/Digraph g)
                       (satisfies? lg/WeightedGraph g)
                       (satisfies? lg/EditableGraph g)
                       (satisfies? la/AttrGraph g)
                       (satisfies? up/Attrs g)
                       (satisfies? up/UndirectedGraph g)
                       (satisfies? up/QueryableGraph g)
                       (satisfies? up/MixedDirectionGraph g))))


(defn edge? "Tests whether o is an edge object"
  [o] (or (instance? Edge o) (instance? UndirectedEdge o)))

(defn- get-edge [g n1 n2] (first (find-edges g n1 n2)))

(defn- add-node [g node]
  (setval [:node-map (if-path (must node) STOP (keypath node))]
          (->NodeInfo {} {} 0 0) g))

(defn- add-node-with-attrs
  "Adds node to g with a given attribute map. Takes a [node attribute-map] pair."
  [g [node attr-map]]
  (add-attrs (add-node g node) node attr-map))

(defn add-nodes-with-attrs*
  "Takes a sequence of [node attr-map] pairs, and adds them to graph g."
  [g nodes-with-attrs]
  (reduce add-node-with-attrs g nodes-with-attrs))

(defn add-nodes-with-attrs
  "Takes any number of [node attr-map] pairs, and adds them to graph g."
  [g & nodes-with-attrs]
  (add-nodes-with-attrs* g nodes-with-attrs))

(defn- remove-node
  [g node]
  (-> g
      (remove-edges* (out-edges g node))
      (remove-edges* (in-edges g node))
      (update :node-map dissoc node)
      (update :attrs dissoc node)))

(def ^:private fconj (fnil conj #{}))
(def ^:private finc (fnil inc 0))

(defn- submap? [m1 m2]
  (every? identity (for [[k v] m1] (= (get m2 k) v))))

(defn- find-edges-impl
  ([g src dest]
   (select-any (keypath :node-map src :out-edges dest) g))
  ([g {src :src dest :dest :as attributes}]
   (let [edges
         (cond
           (and src dest) (select-any (keypath :node-map src :out-edges dest) g)
           src (out-edges g src)
           dest (in-edges g dest)
           :else (edges g))
         attributes (dissoc attributes :src :dest)]
     (if (pos? (count attributes))
       (for [edge edges
             :when (submap? attributes (select-any (keypath :attrs (:id edge)) g))]
         edge)
       edges))))

(defn- find-edge-impl [& args]
  (first (apply find-edges-impl args)))

(defn- add-directed-edge [g src dest attributes]
  (let [g (-> g (add-node src) (add-node dest))
        edge-id (java.util.UUID/randomUUID)
        edge (->Edge edge-id src dest)
        new-attrs (if attributes
                    (assoc (:attrs g) edge-id attributes)
                    (:attrs g))
        node-map (:node-map g)
        new-node-map
        (multi-transform
         (multi-path [(keypath src) (multi-path [(keypath :out-edges dest)
                                                 (terminal #(fconj % edge))]
                                                [:out-degree (terminal finc)])]
                     [(keypath dest) (multi-path [(keypath :in-edges src)
                                                  (terminal #(fconj % edge))]
                                                 [:in-degree (terminal finc)])])
         node-map)]
    (Ubergraph. new-node-map (:allow-parallel? g) (:undirected? g) new-attrs (atom -1))))

(defn- add-undirected-edge [g src dest attributes]
  (let [g (-> g (add-node src) (add-node dest))
        forward-edge-id (java.util.UUID/randomUUID),
        backward-edge-id forward-edge-id,
        forward-edge (->UndirectedEdge forward-edge-id src dest false),
        backward-edge (->UndirectedEdge backward-edge-id dest src true)
        new-attrs (if attributes
                    (assoc (:attrs g) forward-edge-id attributes)
                    (:attrs g))
        node-map (:node-map g)
        new-node-map
        (multi-transform
         (multi-path [(keypath src) (multi-path [(keypath :out-edges dest)
                                                 (terminal #(fconj % forward-edge))]
                                                [(keypath :in-edges dest)
                                                 (terminal #(fconj % backward-edge))]
                                                [:in-degree (terminal finc)]
                                                [:out-degree (terminal finc)])]
                     [(keypath dest) (multi-path [(keypath :in-edges src)
                                                  (terminal #(fconj % forward-edge))]
                                                 [(keypath :out-edges src)
                                                  (terminal #(fconj % backward-edge))]
                                                 [:in-degree (terminal finc)]
                                                 [:out-degree (terminal finc)])])
         node-map)]
    (Ubergraph. new-node-map (:allow-parallel? g) (:undirected? g) new-attrs (atom -1))))

(defn- number->map [n]
  (if (number? n) {:weight n} n))

(defn- add-edge
  [g [src dest attributes :as edge]]
  (if (edge? edge) (throw (ex-info "add-edges takes edge descriptions, not Edge objects. Use `edge-with-attrs` to get edge description from an Edge." {:edge edge}))
      (let [attributes (number->map attributes)]
        (cond
          (and (not (:allow-parallel? g)) (get-edge g src dest))
          (if attributes
            (transform (keypath :attrs (:id (get-edge g src dest)))
                       #(merge % attributes) g)
            g)
          
          (:undirected? g) (add-undirected-edge g src dest attributes)
          :else (add-directed-edge g src dest attributes)))))

(defn- force-add-directed-edge
  [g [src dest attributes :as edge]]
  (if (edge? edge) (throw (ex-info "add-directed-edges takes edge descriptions, not Edge objects. Use `edge-with-attrs` to get edge description from an Edge." {:edge edge}))
      (let [attributes (number->map attributes)]
        (cond
          (and (not (:allow-parallel? g)) (get-edge g src dest))
          (if attributes
            (transform (keypath :attrs (:id (get-edge g src dest)))
                       #(merge % attributes) g)
            g)
          :else (add-directed-edge g src dest attributes)))))

(defn- force-add-undirected-edge
  [g [src dest attributes :as edge]]
  (if (edge? edge) (throw (ex-info "add-undirected-edges takes edge descriptions, not Edge objects. Use `edge-with-attrs` to get edge description from an Edge." {:edge edge}))
      (let [attributes (number->map attributes)]
        (cond
          (and (not (:allow-parallel? g)) (or (get-edge g src dest)
                                              (get-edge g dest src)))
          (let [new-attrs (merge (attrs g src dest) (attrs g dest src) attributes)]
            (-> g
                (remove-edges [src dest] [dest src])
                (add-undirected-edge src dest attributes)))
          :else (add-undirected-edge g src dest attributes)))))

(defn edge-description->edge
  "Many ubergraph functions can take either an *edge description* (i.e., [src dest]
  [src dest weight] or [src dest attribute-map]) or an actual edge object.  This function
  is used to convert edge descriptions into an edge object, or passing through an edge
  object unchanged, so regardless of what you pass in, you're guaranteed to get out
  an edge object."
  [g ed]
  (cond
    (edge? ed) ed
    (not (vector? ed)) (throw (IllegalArgumentException.
                               (str "Invalid edge description: " ed)))
    (= (count ed) 2) (find-edge g (ed 0) (ed 1))
    (= (count ed) 3)
    (cond (number? (ed 2))
          (find-edge g {:src (ed 0), :dest (ed 1), :weight (ed 2)})
          (map? (ed 2))
          (find-edge g (assoc (ed 2) :src (ed 0) :dest (ed 1)))
          :else
          (throw (IllegalArgumentException.
                  (str "Invalid edge description: " ed))))))

(defn- resolve-node-or-edge
  "Similar to edge-description->edge in that it converts edge descriptions to edge objects,
  but this function also passes nodes through unchanged, and extracts the edge id if
  it is an edge."
  [g node-or-edge]
  (cond (edge? node-or-edge) (:id node-or-edge)
        (has-node? g node-or-edge) node-or-edge
        :else
        (try (:id (edge-description->edge g node-or-edge))
             (catch IllegalArgumentException e
               (throw (IllegalArgumentException. (str "Invalid node or edge description: " node-or-edge)))))))

(defn- remove-edge-also-node-if-last-edge [node->edge-set node edge]
  (let [remaining-edges (disj (node->edge-set node) edge)]
    (if (zero? (count remaining-edges))
      (dissoc node->edge-set node)
      (assoc node->edge-set node remaining-edges))))

(defn- remove-edge
  [g edge]
  ;; Check whether edge exists before deleting
  (let [{:keys [src dest id] :as edge} (edge-description->edge g edge)]
    (if (selected-any? (must :node-map src :out-edges dest edge) g)
      (if-let
          [reverse-edge (other-direction g edge)]
        (multi-transform
         (multi-path
          [:attrs (terminal #(dissoc % id))]
          [:node-map
           (multi-path
            [(must src)
             (multi-path [:out-edges
                          (terminal #(remove-edge-also-node-if-last-edge % dest edge))]
                         [:in-edges
                          (terminal #(remove-edge-also-node-if-last-edge
                                      % dest reverse-edge))]
                         [:in-degree (terminal dec)]
                         [:out-degree (terminal dec)])]
            [(must dest)
             (multi-path [:out-edges
                          (terminal #(remove-edge-also-node-if-last-edge
                                      % src reverse-edge))]
                         [:in-edges
                          (terminal #(remove-edge-also-node-if-last-edge % src edge))]
                         [:in-degree (terminal dec)]
                         [:out-degree (terminal dec)])])])
         g)
        (multi-transform
         (multi-path
          [:attrs (terminal #(dissoc % id))]
          [:node-map
           (multi-path
            [(must src)
             (multi-path [:out-edges
                          (terminal #(remove-edge-also-node-if-last-edge % dest edge))]
                         [:out-degree (terminal dec)])]
            [(must dest)
             (multi-path [:in-edges
                          (terminal #(remove-edge-also-node-if-last-edge % src edge))]
                         [:in-degree (terminal dec)])])])
         g))
      g)))

(defn- swap-edge [edge]
  (assoc edge :src (:dest edge) :dest (:src edge)))

(defn- transpose-impl [{:keys [node-map allow-parallel? undirected? attrs reverse-edges]}]
  (let [new-node-map
        (into {} (for [[node {:keys [in-edges out-edges in-degree out-degree]}] node-map
                       :let [new-in-edges
                             (transform [MAP-VALS ALL] swap-edge out-edges),
                             new-out-edges
                             (transform [MAP-VALS ALL] swap-edge in-edges)]]
                   [node (NodeInfo. new-out-edges new-in-edges in-degree out-degree)])),

        new-attrs (into {} (for [[o attr] attrs]
                             (if (edge? o) [(swap-edge o) attr] [o attr])))]

    (Ubergraph. new-node-map allow-parallel? undirected? new-attrs (atom -1))))

(defn add-directed-edges
  "Adds directed edges, regardless of whether the underlying graph is directed or undirected"
  [g & edges]
  (add-directed-edges* g edges))

(defn add-undirected-edges
  "Adds undirected edges, regardless of whether the underlying graph is directed or undirected"
  [g & edges]
  (add-undirected-edges* g edges))

(defn- strip-equal-id-edges
  ([inits] (strip-equal-id-edges (seq inits) #{}))
  ([inits seen-ids]
   (when inits
     (let [init (first inits)]
       (cond
         (edge? init) (if (seen-ids (:id init))
                        (recur (next inits) seen-ids)
                        (cons init (lazy-seq (strip-equal-id-edges
                                              (next inits)
                                              (conj seen-ids (:id init))))))
         :else (cons init (lazy-seq (strip-equal-id-edges
                                     (next inits)
                                     seen-ids))))))))

(defn- nodes-with-attrs [g]
  (for [n (nodes g)] [n (attrs g n)]))

(defn node-with-attrs "Returns [node attribute-map], which can be safely used as an input to build-graph"
  [g node]
  [node (attrs g node)])

(defn edge-with-attrs "Returns [src dest attribute-map] with ^:edge metadata so it can be safely used as an input to build-graph"
  [g edge]
  (let [edge (edge-description->edge g edge)]
    ^:edge [(src edge) (dest edge) (attrs g edge)]))

(defn build-graph
  "Builds graphs using node descriptions of the form node-label or [node-label attribute-map]
  and edge descriptions of the form [src dest], [src dest weight], or [src dest attribute-map].
  Also can build from other ubergraphs and from adjacency maps.

  Use ^:node and ^:edge metadata to resolve ambiguous inits, or build your graph with the more
  precise add-nodes, add-nodes-with-attrs, and add-edges functions."
  [g & inits]
  (letfn [(build [g init]
            (cond
              ;; ubergraph
              (instance? Ubergraph init)
              (let [new-g (add-nodes-with-attrs* g (nodes-with-attrs init)),
                    directed-edges (for [e (edges init)
                                         :when (directed-edge? e)]
                                     [(src e) (dest e) (attrs init e)])
                    undirected-edges (for [e (edges init),
                                           :when (and (undirected-edge? e)
                                                      (not (mirror-edge? e)))]
                                       [(src e) (dest e) (attrs init e)])
                    new-g (add-directed-edges* new-g directed-edges)
                    new-g (add-undirected-edges* new-g undirected-edges)]
                new-g)

              ;; Adding Edge objects is deprecated in version 0.7.0
              ;; Use edge descriptions instead
              (edge? init)
              (throw (ex-info "See build-graph docstring for valid inits. Use `edge-with-attrs` to get edge description from an Edge." {:init init}))

              ;; (directed-edge? init)
              ;; (let [new-g (add-nodes g (src init) (dest init)),
              ;;       new-g (add-directed-edges g [(src init) (dest init)
              ;;                                    (attrs (meta init) init)])]
              ;;   new-g)

              ;; (undirected-edge? init)
              ;; (let [new-g (add-nodes g (src init) (dest init)),
              ;;       new-g (add-undirected-edges g [(src init) (dest init)
              ;;                                      (attrs (meta init) init)])]
              ;;   new-g)

              ;; Marked as a node
              (:node (meta init))
              (add-node g init)

              ;; Marked as an edge
              (:edge (meta init))
              (let [[src dest n] init]
                (add-edge g [src dest (number->map n)]))

              ;; Adjacency map
              (map? init)
              (let [es (if (map? (val (first init)))
                         (for [[n nbrs] init
                               [nbr wt] nbrs]
                           [n nbr wt])
                         (for [[n nbrs] init
                               nbr nbrs]
                           [n nbr]))]
                (-> g
                    (add-nodes* (keys init))
                    (add-edges* es)))

              ;; node-with-attributes
              (and (vector? init) (= 2 (count init)) (map? (init 1)))
              (add-node-with-attrs g [(init 0) (init 1)])

              ;; edge description
              (and (vector? init) (#{2,3} (count init)))
              (add-edge g [(init 0) (init 1) (number->map (get init 2))])

              ;; node
              :else (add-node g init)))]
    (reduce build g (strip-equal-id-edges inits))))

;; All of these graph options can also serve as weighted graphs, just initialize accordingly.

(defn multigraph
  "Multigraph constructor. See build-graph for description of valid inits"
  [& inits]
  (apply build-graph (->Ubergraph {} true true {} (atom -1)) inits))

(defn multidigraph
  "Multidigraph constructor. See build-graph for description of valid inits"
  [& inits]
  (apply build-graph (->Ubergraph {} true false {} (atom -1)) inits))

(defn graph
  "Graph constructor. See build-graph for description of valid inits"
  [& inits]
  (apply build-graph (->Ubergraph {} false true {} (atom -1)) inits))

(defn digraph
  "Digraph constructor. See build-graph for description of valid inits"
  [& inits]
  (apply build-graph (->Ubergraph {} false false {} (atom -1)) inits))

(defn ubergraph
  "General ubergraph construtor. Takes booleans for allow-parallel? and undirected? to
  call either graph, digraph, multigraph, or multidigraph.
  See build-graph for description of valid inits"
  [allow-parallel? undirected? & inits]
  (apply build-graph (->Ubergraph {} allow-parallel? undirected? {} (atom -1)) inits))

;; Serialize/deserialize to an edn Clojure data structure

(defn ubergraph->edn [g]
  {:allow-parallel? (:allow-parallel? g),
   :undirected? (:undirected? g),
   :nodes (vec (for [node (nodes g)] [node (attrs g node)]))
   :directed-edges (vec (for [edge (edges g) :when (directed-edge? edge)]
                          [(src edge) (dest edge) (attrs g edge)]))
   :undirected-edges (vec (for [edge (edges g) :when (and (undirected-edge? edge) (not (mirror-edge? edge)))]
                            [(src edge) (dest edge) (attrs g edge)]))})

(defn edn->ubergraph [{:keys [allow-parallel? undirected? nodes directed-edges undirected-edges]}]
  (-> (ubergraph allow-parallel? undirected?)
      (add-nodes-with-attrs* nodes)
      (add-directed-edges* directed-edges)
      (add-undirected-edges* undirected-edges)))

;; Override print-dup so we can serialize to a string with (binding [*print-dup* true] (pr-str my-graph))
;; Deserialize from string with read-string.

(defmethod print-dup ubergraph.core.Ubergraph [o w]
  (print-ctor o (fn [o w] (print-dup (:node-map o) w) (.write w " ") (print-dup (:allow-parallel? o) w) (.write w " ") (print-dup (:undirected? o) w) (.write w " ") (print-dup (:attrs o) w) (.write w " ")
                  (print-ctor (:cached-hash o) (fn [o w] (print-dup (:cached-hash o) w)) w)) w))

;; Friendlier printing

(defn- graph-type [g]
  (cond
    (and (:allow-parallel? g) (:undirected? g)) "Multigraph"
    (:allow-parallel? g) "Multidigraph"
    (:undirected? g) "Graph"
    :else "Digraph"))

(defn count-nodes "Counts how many nodes are in g" [g]
  (if (instance? Ubergraph g)
    (count (:node-map g))
    (count (nodes g))))

(defn count-edges "Counts how many edges are in g.
  Undirected edges are counted twice, once for each direction."
  [g]
  (apply + (for [node (nodes g)]
             (out-degree g node))))

(defn count-unique-edges "Counts how many edges are in g.
  Undirected edges are counted only once."
  [g]
  (count (for [edge (edges g)
               :when (not (mirror-edge? edge))]
           edge)))

(defn pprint
  "Pretty print an ubergraph"
  [g]
  (println (graph-type g))
  (println (count-nodes g) "Nodes:")
  (doseq [node (nodes g)]
    (println \tab node (let [a (attrs g node)] (if (seq a) a ""))))
  (println (count-unique-edges g) "Edges:")
  (doseq [edge (edges g)]
    (cond
      (directed-edge? edge)
      (println \tab (src edge) "->" (dest edge)
               (let [a (attrs g edge)]
                 (if (seq a) a "")))
      (and (undirected-edge? edge) (not (mirror-edge? edge)))
      (println \tab (src edge) "<->" (dest edge)
               (let [a (attrs g edge)]
                 (if (seq a) a ""))))))

;; For Codox, don't want to document these constructors
(alter-meta! #'->Edge assoc :no-doc true)
(alter-meta! #'->NodeInfo assoc :no-doc true)
(alter-meta! #'->Ubergraph assoc :no-doc true)
(alter-meta! #'->UndirectedEdge assoc :no-doc true)
(alter-meta! #'map->Edge assoc :no-doc true)
(alter-meta! #'map->NodeInfo assoc :no-doc true)
(alter-meta! #'map->UndirectedEdge assoc :no-doc true)

;; Equality is more complicated
;; We can't use Clojure's default structural equality because
;; of the randomness of UUIDs.  The same graph, created at
;; different points in time will have different UUIDs for the same edges
;; so they won't compare as equal.
;; We compare for equality by stripping out UUIDs and
;; matching up edges between the same src and dest, with
;; the same directedness and same attributes.

(defn- edge-characteristics [g]
  (fn [edge]
    [(directed-edge? edge)
     (attrs g edge)
     (src edge)
     (dest edge)]))

(defn- equal-edges? [g1 g2 n1 n2]
  (let [edges-between-g1 (find-edges g1 n1 n2),
        edge-characteristics-g1 (frequencies (map (edge-characteristics g1) edges-between-g1))
        edges-between-g2 (find-edges g2 n1 n2),
        edge-characteristics-g2 (frequencies (map (edge-characteristics g2) edges-between-g2))]
    (= edge-characteristics-g1 edge-characteristics-g2)))

(defn- edges-freqs [g]
  (frequencies (map (edge-characteristics g) (edges g))))

(defn- node-attrs [g]
  (let [g-attrs (:attrs g)]
    (if (pos? (count g-attrs)) ; only bother with this step if graph has attributes
      (into {} (for [n (nodes g) :let [a (get g-attrs n {})] :when (seq a)]
                 [n a]))
      {})))

(defn- equal-nodes?
  "Assumes that we've already established (= (nodes g1) (nodes g2)).
  We're just checking the attributes here"
  [g1 g2]
  (let [g1-attrs (:attrs g1), g2-attrs (:attrs g2)]
    (if
        (and (zero? (count g1-attrs))
             (zero? (count g2-attrs)))
      true
      (every? identity
              (for [n (nodes g1)]
                (= (get g1-attrs n {})
                   (get g2-attrs n {})))))))

(defn- node-set [^Ubergraph g]
  (let [^java.util.Map m (:node-map g)]
    (.keySet m)))

(defn- equal-graphs? [^Ubergraph g1 ^Ubergraph g2]
  (or (.equals g1 g2)
      (and
       (or
        (= @(:cached-hash g1) -1)
        (= @(:cached-hash g2) -1)
        (= @(:cached-hash g1) @(:cached-hash g2)))
       (= (count-nodes g1) (count-nodes g2))
       (= (node-set g1) (node-set g2))
       (= (count-edges g1) (count-edges g2))
       (equal-nodes? g1 g2)
       (every? identity
               (for [node1 (nodes g1),
                     node2 (successors g1 node1)]
                 (equal-edges? g1 g2 node1 node2))))))

(defn- hash-graph [g]
  (let [h (:cached-hash g)
        val @h]
    (if (= val -1)
      (let [ns (node-set g),
            code (hash {:nodes (hash-unordered-coll ns),
                        :node-attrs (node-attrs g),
                        :edges (edges-freqs g)})]
        (reset! h code)
        code)
      val)))

;; Visualization

;; Dorothy doesn't like attribute maps with values other than numbers, strings, and keywords

(defn- valid-dorothy-id? [x]
  (or (keyword? x) (string? x) (number? x)))

(defn- remove-invalids-from-map [m]
  (into (empty m) (for [[k v] m :when (and (valid-dorothy-id? k)
                                           (valid-dorothy-id? v))]
                    [k v])))

(defn- sanitize-attrs [g i]
  (remove-invalids-from-map (attrs g i)))

;; Dorothy has a bug - it doesn't escape backslashes, so we do it here.
;; Also replace several other special characters that cause problems
;; for Graphviz dot when they appear as part of labels.
(def ^:private dorothy-label-char-replacement-map
  {(char 0) "\\\\u0000"
   (char 65534) "\\\\ufffe"
   (char 65535) "\\\\uffff"
   \\ "\\\\"})

(defn escape-label
  "Takes a string intended to be used as a node or edge label in a
  Graphviz graph, and returns a string with several characters that
  cause problems for Graphviz with suitable replacements.  Called for
  you automatically if you use the ':auto-label true' option of the
  viz-graph function, but you may want to use it for label strings you
  provide to viz-graph when you do not use that option."
  [s]
  (clojure.string/escape s dorothy-label-char-replacement-map))

(defn- label [g]
  (as-> g $
    (reduce
     (fn [g n]
       (add-attr g n :label (str (if (keyword? n) (subs (str n) 1) n)
                                 \newline
                                 (escape-label (with-out-str (clojure.pprint/pprint (attrs g n)))))))
     $ (nodes g))
    (reduce
     (fn [g e]
       (if (not (mirror-edge? e))
         (add-attr g e :label (escape-label (with-out-str (clojure.pprint/pprint (attrs g e)))))
         g))
     $ (edges g))))

(defn- dotid [n]
  (if (or (string? n)
          (keyword? n)
          (number? n))
    n
    (str/replace (print-str n) ":" "")))

(defn viz-graph
  "Uses graphviz to generate a visualization of your graph. Graphviz
  must be installed on your computer and in your path. Passes along
  to graphviz the attributes on the nodes and edges, so graphviz-related
  attributes such as color, style, label, etc. will be respected.

  Takes an optional map which can contain:
  :auto-label true (labels each node/edge with its attribute map)
  :layout :dot, :neato, :fdp, :sfdp, :twopi, or :circo
  :save {:filename _, :format _} where format is one of
  :bmp :dot :eps :gif :ico :jpg :jpeg :pdf :png :ps :ps2 :svgz :tif :tiff :vmlz :wbmp
  Additionally map can contain graph attributes for graphviz like :bgcolor, :label, :splines, ..."
  ([g] (viz-graph g {}))
  ([g {layout :layout {filename :filename format :format :as save} :save
       auto-label :auto-label
       :as opts
       :or {layout :dot}}]
   (let [g (if auto-label (label g) g)
         ns (nodes g),
         es (edges g)
         nodes (for [n ns]
                 [(dotid n)
                  (sanitize-attrs g n)]),
         directed-edges (for [e es :when (directed-edge? e)]
                          [(dotid (src e)) (dotid (dest e)) (sanitize-attrs g e)])
         undirected-edges (for [e es :when (and (undirected-edge? e)
                                                (not (mirror-edge? e)))]
                            [(dotid (src e)) (dotid (dest e))
                             (merge {:dir :none} (sanitize-attrs g e))])]
     (-> (concat [(merge {:layout layout} (dissoc opts :layout :save :auto-label))]
                 nodes directed-edges undirected-edges)
         d/digraph
         d/dot
         (cond->
             (and save (= :dot format)) (#(spit filename %))
             (and save (not= :dot format)) (dj/save! filename {:format format})
             (not save) dj/show!)))))
