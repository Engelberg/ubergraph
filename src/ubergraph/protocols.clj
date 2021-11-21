(ns ubergraph.protocols)

; NEW CONCEPTS

; There are three new protocols above and beyond what is in Loom:
; UndirectedGraph for undirected graphs
; QueryableGraph for attribute graphs and multigraphs
; MixedDirectionGraph for graphs which support a mixture of directed and undirected edges
;    within the same graph

; For undirected graph algorithms, it is useful to know the connection between the
; two sides of an undirected edge, so we can keep attributes and weight in sync
; and add/remove or otherwise mark them in sync.
(defprotocol UndirectedGraph
  (other-direction [g edge]
  "Returns the other direction of an undirected edge in graph g.  If
  edge is a directed edge, returns nil."))

; We need a way to retrieve edges that is friendly to both regular and multi-graphs,
; but especially important for multi-graphs.
(defprotocol QueryableGraph
  (find-edges [g src dest] [g query]
    "Returns all edges that match the query.

  (find-edges g src dest) and (find-edges g {:src src :dest dest})
  return a sequence of edges in the graph g that are from node src to
  node dest.  In graphs where parallel edges are allowed, there can be
  more than one edge in this sequence.

  If src and dest are different nodes, the sequence includes all
  directed edges from src to dest once each, and all undirected edges
  between src and dest once each.

  If src and dest are the same node, directed 'self loop' edges are
  included once each, but undirected 'self loop' edges are included
  twice, one with mirror-edge? returning true, the other false.

  (find-edges g {:src src}) without :dest is the same as (out-edges g
  src), (find-edges g {:dest dest}) without :src is the same
  as (in-edges g dest).  (find-edges g {}) is the same as (edges g).

  Any keys other than :src or :dest in the map will be used to filter
  the sequence of edges to only those that have at least those keys in
  their attribute map, and equal associated values as those in the
  query map.

  Examples:

  (find-edges g {:src 1, :dest 3, :color :red})
     finds all edges from node 1 to node 3 with :color :red in edge
     attributes.

  (find-edges g {:src 1, :weight 5})
     finds all edges from node 1 with a weight of 5.

  (find-edges g {:dest :Chicago, :airline :Delta, :time :afternoon})
     finds all edges leading to node :Chicago with :airline :Delta and
     :time :afternoon in edge attributes.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time, with effectively O(k) time to traverse all k edges in the
  sequence.  The implementation traverses all k edges, internally
  filtering for the ones that match the attribute part of a query, so
  must traverse all edges that do not satisfy the attribute
  conditions, even though they are not included in the returned
  sequence.")
  (find-edge [g src dest] [g query]
    "Returns first edge that matches the query, same
  as (first (find-edges ...)).  Returns nil if no edges match the
  query.

  For the implementation on type Ubergraph, returns in effectively
  O(1) time when there are no attributes to match.  When there are
  attributes to match, returns in effectively O(k) time, where k is
  the number of edges traversed until the first edge matching the
  query is found."))

; It would be nice to have a way to incorporate both undirected and directed edges
; into the same graph structure.

(defprotocol MixedDirectionEdgeTests
  (undirected-edge? [e] "Is e one 'direction' of an undirected edge?")
  (directed-edge? [e] "Is e a directed edge?")
  (mirror-edge? [e] "Is e the mirrored half of the undirected edge?"))

(defprotocol MixedDirectionGraph
  (add-directed-edges* [g edges] "Adds directed edges regardless of the graph's undirected/directed default")
  (add-undirected-edges* [g edges] "Adds undirected edges regardless of the graph's undirected/directed default"))

; Improved attribute handling

(defprotocol Attrs
  (add-attrs [g node-or-edge attribute-map] [g n1 n2 attribute-map] "Merges an attribute map with the existing attributes of a node or edge")
  (set-attrs [g node-or-edge attribute-map] [g n1 n2 attribute-map] "Sets the attribute map of a node or edge, overwriting existing attribute map")  
  (remove-attrs [g node-or-edge attributes] [g n1 n2 attributes] "Removes the attributes from the node or edge"))

; Path protocols

(defprotocol IPath
  "All the things you can do to a path"
  (edges-in-path [path] "A list of edges comprising the path" )
  (nodes-in-path [path] "A list of nodes comprising the path" )
  (cost-of-path [path] "Returns the cost of the path with respect to the property that was minimized
in the search that produced this path." )
  (start-of-path [path] "Returns the first node in the path")
  (end-of-path [path] "Returns the last node in the path")
  (last-edge-of-path [path] "Returns the last edge in the path"))

(defprotocol IAllPathsFromSource
  "An object that knows how to produce paths on demand from a given source,
  using path-to"
  (path-to [paths dest] "The shortest path to dest")
  (all-destinations [paths] "All possible destinations we know how to get to"))

(defprotocol IAllPaths "An object that knows how to produce paths on demand between any pair of nodes,
using path-between"
  (path-between [paths src dest] "The shortest path between src and dest" ))

; Recognizing Ubergraphs

(defprotocol IUbergraph "Is it an Ubergraph?"
  (ubergraph? [g]))
