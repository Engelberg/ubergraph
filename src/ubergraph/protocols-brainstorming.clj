(ns ubergraph.protocols-brainstorming)

(defprotocol Graph
  (nodes [g] "Return a collection of the nodes in graph g")
  ; Add ability to enumerate all edges between n1 and n2
  (edges [g] [g n1 n2] "Edges in g. May return each edge twice in an undirected graph
                        (edges g n1 n2) returns all edges from n1 to n2")
  (has-node? [g node] "Return true when node is in g")
  (has-edge? [g n1 n2] "Return true when an edge from n1 to n2 is in g")
  (successors [g] [g node] "Return direct successors of node, or (partial successors g)")
  (out-degree [g node] "Return the number of direct successors of node")
  ; new
  (out-edges [g] [g node] "Return outgoing edges of node, or (partial out-edges g)"  )
  )

; Note that with the above extension to the edges protocol function,
; has-edge? can now be implemented as (seq (edges g n1 n2)).
; It doesn't necessarily need to be part of the Graph protocol.

; Still don't like the name Digraph for this, maybe Bigraph would be better
(defprotocol Digraph
  (predecessors [g] [g node] "Return direct predecessors of node, or (partial predecessors g)")
  (in-degree [g node] "Return the number direct predecessors to node")  
  (transpose [g] "Return a graph with all edges reversed")
  ; new
  (in-edges [g] [g node] "Return the incoming edges of node, or (partial in-edges g)")  
)
  
; Loom currently hardcodes the notion of edge to be a [node1 node2] pair.
; This is not good enough for multigraphs with weights and attributes.
; An Edge is something from which you can get the src and dest.
; Let's represent this as a protocol, so we are not tied to a specific representation.
; That way, we have the option to store richer information in the edge if we so choose.
  
; NEW PROTOCOL
(defprotocol Edge
  (src [edge] "Return the source end of the edge")
  (dest [edge] "Return the destination end of the edge"))

; Modify the weight protocol function to take an edge
; (weight n1 n2) remains for backwards compatibility, but would be discouraged
; since algorithms using that form won't work properly with multigraphs.
; Or scrap the WeightedGraph protocol altogether (see my comment at the bottom of the file)

(defprotocol WeightedGraph
  (weight [g] [g e] [g n1 n2] 
       "Return weight of edge e, or arbitrary edge from n1 to n2, or (partial weight g)"))

(defprotocol EditableGraph
  (add-nodes* [g nodes] "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges] "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g] "Removes all nodes and edges from graph g"))

; The old AttrGraph protocol is insufficient.

;(defprotocol AttrGraph
;  (add-attr [g node k v] [g n1 n2 k v] "Add an attribute to node or edge")
;  (remove-attr [g node k] [g n1 n2 k] "Remove an attribute from a node or edge")
;  (attr [g node k] [g n1 n2 k] "Return the attribute on a node or edge")
;  (attrs [g node] [g n1 n2] "Return all attributes on a node or edge"))

; One option is to make the attr protocol functions take a node or edge.
; Implementations could test for implementation of Edge protocol to discriminate.
; Could also keep around the n1,n2 forms of the protocol functions for backwards
; comaptibility, or where you know you don't have multiple edges between a pair of nodes.
(defprotocol AttrGraph
  (add-attr [g node-or-edge k v] [g n1 n2 k v] "Add an attribute to node or edge")
  (remove-attr [g node-or-edge k] [g n1 n2 k] "Remove an attribute from a node or edge")
  (attr [g node-or-edge k] [g n1 n2 k] "Return the attribute on a node or edge")
  (attrs [g node-or-edge] [g n1 n2] "Return all attributes on a node or edge"))

; Another option is to have separate protocol functions for nodes and edges:
(defprotocol AttrGraph
  (add-node-attr [g node k v] "Add an attribute to node")
  (remove-node-attr [g node k] "Remove an attribute from a node")
  (node-attr [g node k] "Return the attribute on a node")
  (node-attrs [g node] "Return all attributes on a node")
  (add-edge-attr [g edge k v] [g n1 n2 k v] "Add an attribute to edge")
  (remove-edge-attr [g edge k] [g n1 n2 k] "Remove an attribute from edge")
  (edge-attr [g edge k] [g n1 n2 k] "Return the attribute on edge")
  (edge-attrs [g edge] [g n1 n2 k] "Return all attributes on edge"))

; NEW CONCEPTS

; For undirected graph algorithms, it is useful to know the connection between the
; two sides of an undirected edge, so we can keep attributes and weight in sync
; and add/remove or otherwise mark them in sync.
(defprotocol UndirectedEdge
  (reverse-edge [g edge] "Returns the other direction of this edge in graph g"))

; We need a way to retrieve edges that is friendly to both regular and multi-graphs,
; but especially important for multi-graphs.
(defprotocol Query
  (find-edges [g query] "Returns all edges that match the query")
  (find-edge [g query] "Returns first edge that matches the query"))

; Sample queries
; (find-edges g {:src 1, :dest 3, :color :red})  
;    finds all edges from 1 to 3 with :color :red in edge attributes
;
; (find-edges g {:src 1, :weight 5})
;    finds all edges from 1 with a weight of 5
;
; (find-edges g {:dest :Chicago, :airline :Delta, :time :afternoon})
;    finds all edges leading to :Chicago with :airline :Delta and 
;    :time :afternoon in edge attributes

; OTHER COMMENTS

; I think it might be better if the edges protocol function guaranteed 
; returning each edge once in an undirected graph (i.e., only returns one side).
; Need to experiment more with implementing undirected algorithms to be sure.

; I know one of the FAQs is "Why is weight not an attribute?"
; I've read that, but I still think the WeightedGraphProtocol is misguided.

; The problem is that it makes an assumption that weight is an unchanging property of
; graphs and edges.  It fails to take into account, for example, that I might want to
; first do a shortest-path based on :miles, and then turn around and do a
; shortest-path on the same graph based on :price, then do another search where
; the weight is computed as a function of the graph and edge.
;
; In my opinion, the WeightedGraphProtocol is unnecessary and actively
; hinders this kind of flexibility in the algorithms.  Instead,
; all of the algorithms that require weight should take it as an input, where the
; weight input is either an attribute name (stored in the attribute map) or
; something of the form (fn [graph edge] weight).

; If you really want to keep the WeightedGraphProtocol, I'd still 
; recommend that this just be considered the "default weight" and the
; weighted search algorithms should allow an optional keyword argument 
; to override the "default weight" with either an attribute name or
; (fn [graph edge] weight)

; So, for example, the following function...

(defn customize-weight [g custom]
  (cond
    (nil? custom) (fn [edge] (weight g edge))
    (keyword? custom) (fn [edge] (get (attrs g edge) custom))
    :else (fn [edge] (custom g edge))))

; ... could be used as a helper function to allow for overriding

(defn shortest-path [g n1 n2 {custom :weight}]
  (let [weight (customize-weight g custom)]
    ...))

; Now you could call things like

(shortest-path g 1 3)
(shortest-path g 1 3 :weight :distance)
(shortest-path g 1 4 :weight :price)
(shortest-path g 2 5 :weight (fn [g edge] (+ (src edge) (dest edge))))