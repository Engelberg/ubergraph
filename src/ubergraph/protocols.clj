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
  (other-direction [g edge] "Returns the other direction of this edge in graph g"))

; We need a way to retrieve edges that is friendly to both regular and multi-graphs,
; but especially important for multi-graphs.
(defprotocol QueryableGraph
  (find-edges [g src dest] [g query] "Returns all edges that match the query")
  (find-edge [g src dest] [g query] "Returns first edge that matches the query"))

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

; It would be nice to have a way to incorporate both undirected and directed edges
; into the same graph structure.

(defprotocol MixedDirectionEdgeTests
  (undirected-edge? [e] "Is e one 'direction' of an undirected edge?")
  (directed-edge? [e] "Is e a directed edge?")
  (mirror-edge? [e] "Is e the mirrored half of the undirected edge?"))

(defprotocol MixedDirectionGraph
  (add-directed-edges* [g edges] "Adds directed edges regardless of the graph's undirected/directed default")
  (add-undirected-edges* [g edges] "Adds undirected edges regardless of the graph's undirected/directed default"))


; Path protocols

(defprotocol IPath "All the things you can do to a path"
  (edges-in-path [path] "A list of edges comprising the path" )
  (nodes-in-path [path] "A list of nodes comprising the path" )
  (cost-of-path [path] "Returns the cost of the path with respect to the property that was minimized
in the search that produced this path." ))

(defprotocol IAllPathsFromSource "All the things you can do to an object that knows how to produce 
paths on demand from a given source."
  (edges-in-path-to [path dest] "A list of all edges comprising the path to dest" )
  (nodes-in-path-to [path dest] "A list of all nodes comprising the path to dest" )
  (cost-of-path-to [path dest] "Cost of the path to dest with respect to the property that was minimized
in the search that produced this path." ))

(defprotocol IAllPaths "All the things you can do to an object that knows how to produce paths
on demand between any pair of nodes."
  (edges-in-path-between [path src dest] "A list of all edges along the path between src and dest" )
  (nodes-in-path-between [path src dest] "A list of all nodes along the path between src and dest" )
  (cost-of-path-between [path src dest] "Cost of path between src and dest with respect to the property that was minimized 
in the search that produced this path."))

