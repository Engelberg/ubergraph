# Ubergraph

Ubergraph is a versatile, general-purpose graph data structure for Clojure.  It is designed to complement and extend [Loom](https://github.com/aysylu/loom), a popular Clojure collection of graph protocols and algorithms.

## Features

* Ubergraph supports directed edges, undirected edges, weighted edges, node and edge attributes.
* Ubergraph implements all of Loom's protocols.
* Ubergraph goes beyond Loom's protocols, allowing a mixture of directed and undirected edges within a single graph,
multiple "parallel" edges between a given pair of nodes, multiple weights per edge, and changeable weights.

Ubergraph is a great choice for people who:

* Want to use Loom, but don't want to think about which specific Loom graph implementation to use.  (Hmmm, do I need directed edges or undirected edges?  Do I need weights or attributes for this project?  I'm not sure yet, so I'll just use ubergraph because it can do it all.)
* Need graph capabilities beyond what Loom provides.
* Are implementing algorithms for Loom, and want to test the algorithm against an alternative graph implementation to be certain you've properly programmed the algorithm against the necessary abstractions, rather than Loom's concrete representations.

## Quickstart

Add the following line to your leiningen dependencies:

	[ubergraph "0.1.0"]

Require ubergraph in your namespace header:

	(ns example.core
	  (:require [ubergraph.core :as uber]))

Ubergraph lists loom as a dependency, so when you add ubergraph to your project, leiningen will also download loom.  This means all of loom's namespaces are also available to you in your program.  Loom is currently organized in a way that its protocols are split across several different namespaces.  As a convenience, ubergraph.core provides access to all of loom's protocol functions through its own namespace.

For example, rather than calling `loom.graph/out-edges` and `loom.attr/add-attr`, you can just call `uber/out-edges` and `uber/add-attr`.

## Usage

There are four flavors of Ubergraphs: graph, digraph, multigraph, and multidigraph.  All share the same underlying representation, but have different default behaviors with respect to adding new edges and attributes.  Specifically:

Graphs and Digraphs do not allow duplicate or "parallel" edges, whereas Multigraphs and Multidigraphs do.

Digraphs and Multidigraphs, by default, treat new edge definitions as directed/one-way edges, whereas Graphs and Multigraphs, by default, treat new edge definitions as bidirectional.  It is possible, however, to override this default behavior and add directed edges to a an undirected graph, and undirected edges to a directed graph.

<table>
  <tr>
    <th></th>
    <th>Allows parallel edges<br></th>
    <th>No parallel edges<br></th>
  </tr>
  <tr>
    <th>Directed Edges (by default)<br></th>
    <td>Multidigraph</td>
    <td>Digraph</td>
  </tr>
  <tr>
    <th>Undirected Edges (by default)<br></th>
    <td>Multigraph</td>
    <td>Graph</td>
  </tr>
</table>

All ubergraph constructors are multiple arity functions that can take an arbitrary number of "inits" where an init is defined as one of:

+ Edge description:
	+ [src dest]
	+ [src dest weight]
	+ [src dest attribute-map]
+ Adjacency map (e.g., {1 [2 3], 2 [3]} adds the edges 1->2, 1->3, and 2->3).
+ Weighted adjacency map (e.g., {:a {:b 2, :c 3}} creates an edge of weight 2 between :a and :b, etc.)
+ Another ubergraph
+ Node (anything that doesn't fit one of the other patterns is interpreted as a node)

### Graphs

Ubergraphs built with the graph constructor treat every edge as a bidirectional, undirected edge.

```clojure
(def graph1
  (uber/graph [:a :b] [:a :c] [:b :d]))

=> (uber/pprint graph1)
Graph
4 Nodes:
	 :d
	 :c
	 :b
	 :a
3 Edges:
	 :b <-> :d
	 :a <-> :c
	 :a <-> :b
```

Edge definitions can include weights.

```clojure
(def graph2
  (uber/graph [:a :b 2] [:a :c 3] [:b :d 4]))

=> (uber/pprint graph2)
Graph
4 Nodes:
	 :d
	 :c
	 :b
	 :a
3 Edges:
	 :b <-> :d {:weight 4}
	 :a <-> :c {:weight 3}
	 :a <-> :b {:weight 2}
```

Note that ubergraph differs from Loom in the way that it handles weights.  *Ubergraph simply stores weights in the edge's attribute map, under the keyword :weight.*  In my opinion, this is a superior way to handle weights because it allows you to manipulate weights using the same interface that you use to alter other attributes.  Also, once weight is no longer a privileged field, it makes it easier to develop algorithms that take as an additional input the attribute to use as the edge weight.  Right now, Loom algorithms have a baked-in notion that we only want to do traversals based on weight, and this is a problem.  Ideally, we want it to be just as easy to search a graph for shortest traversals using other attributes such as :cost or :distance.  However, to be compatible with Loom's protocols, Ubergraphs support the protocol function `weight` which simply extracts the `:weight` attribute from the attribute map.

```clojure
(def graph3
  (uber/graph [:a :b {:weight 2 :cost 200 :distance 10}]
              [:a :c {:weight 3 :cost 300 :distance 20}]))

=> (uber/pprint graph3)
Graph
3 Nodes:
	 :c
	 :b
	 :a
2 Edges:
	 :a <-> :c {:weight 3, :cost 300, :distance 20}
	 :a <-> :b {:weight 2, :cost 200, :distance 10}
```

You can extend graphs with more "inits" by using `build-graph`, or you can use `add-nodes` or `add-edges` (which takes any legal edge descriptors).  `add-nodes*` and `add-edges*` are variants which take a sequence of nodes/edges rather than multiple args.

```clojure
(build-graph graph1 [:d :a] [:d :e])
(add-edges graph1 [:d :a] [:d :e])
(add-edges* graph1 [[:d :a] [:d :e]])
(add-nodes graph1 :e)
(add-nodes* graph1 [:e])
```

Adding nodes that already exist will do nothing.  Graphs do not permit "parallel edges", so adding edges that already exist will cause the new attribute map to be merged with the existing one.  However, if you really want to, you can add a directed edge to an undirected graph with `add-directed-edges` or `add-directed-edges*`:

```clojure
=> (uber/pprint (uber/add-directed-edges graph1 [:a :d]))
Graph
4 Nodes:
	 :d
	 :c
	 :b
	 :a
4 Edges:
	 :b <-> :d
	 :a -> :d
	 :a <-> :c
	 :a <-> :b
```

Ubergraph supports all of Loom's protocols, so you can do all the things you'd expect to be able to do to graphs:
nodes, edges, has-node?, has-edge?, successors, out-degree, out-edges, predecessors, in-degree, in-edges, transpose, weight, add-nodes, add-nodes\*, add-edges, add-edges\*, remove-nodes, remove-nodes\*, remove-edges, remove-edges\*, and remove-all.

Ubergraph also supports the ability to lookup, add, and remove attributes to any node or edge in the graph via: add-attr, remove-attr, attr, and attrs.

#### Edges

One way that Ubergraph improves upon Loom is that graph edges have a richer implementation and support additional abstractions.  This richer implementation is what allows directed and undirected edges to coexist, and allows for parallel edges in multigraphs.  So where Loom functions return [src dest] tuples or [src dest weight] tuples to represent edges, Ubergraph returns actual Edge or UndirectedEdge objects.  For example,

```clojure
=> (-> (uber/graph [:a :b])
     (uber/add-directed-edges [:a :c])
     uber/edges)

({:id #uuid "4036b8a8-4ac2-40bd-ac13-051dac20b77d",
  :src :b,
  :dest :a,
  :mirror? true}
 {:id #uuid "15444a68-9da6-499c-a887-b16955e2740b", :src :a, :dest :c}
 {:id #uuid "4036b8a8-4ac2-40bd-ac13-051dac20b77d",
  :src :a,
  :dest :b,
  :mirror? false})
```

The main thing to note here is that internally, all edges have a `:src` field, a `:dest` field, and a uuid, which you can think of as a pointer to the map of attributes for that edge.  The other thing to note is that undirected edges are stored internally as a pair of edge objects, one for each direction.  Both edges of the pair share the same id (and therefore, the same attribute map) and one of the edges is marked as a "mirror" edge.  This is critical because in some algorithms, we want to traverse over all edges in both directions, but in other algorithms we only want to traverse over unique edges.  Loom provides no mechanism for this, but Ubergraph makes this easy with the protocol function `mirror-edge?`, which returns true for the mirrored edge in an undirected pair of edges, and false for directed edges and the non-mirrored undirected edges.  So `(edges g)` gives you all the edges in a graph, and `(remove mirror-edge? (edges g))` would give you a sequence of unique edges, without listing both directions of the same undirected edge.

When writing algorithms over edges, it is strongly recommended that you access the edge endpoints through the edge abstraction, using the protocol functions `src` and `dest` (although edges support [src dest] and [src dest weight] destructuring for backwards compatibility with Loom).  Edges also support the protocol functions `edge?`, `directed-edge?`, `undirected-edge?`, and `other-direction` (which returns the other edge in the pair for undirected edges, or nil if it is a directed edge).

### Digraphs

Digraphs (short for "directed graphs") behave similarly to Graphs, but by default, they add edges as one-way directed edges.  All the same functions apply to directed graphs.  You can add undirected edges to a directed graph with `add-undirected-edges` or `add-undirected-edges*`.

### Multigraphs

Multigraphs allow "parallel edges" between pairs of nodes.  This is where Ubergraph really shines over Loom.  The crux of the problem with Loom is that throughout Loom's implementation, the protocols and algorithms are hardcoded with the notion that an edge is uniquely described by its source and destination.  This completely breaks down when you're dealing with multigraphs.

We've already seen one way that Ubergraph solves this problem: Ubergraph identifies edges not only by a src and a dest, but also by a unique id.  This allows it to store multiple edges between a given src and dest, each with its own id which can be used to look up its set of attributes.

But there are also many functions which require some description of an edge as an input.  Describing an edge as a [src dest] pair is not enough.  When multiple edges can have the same src and dest, we need a richer way to identify a given edge.

#### Edge Descriptions

Ubergraph introduces the notion of an "edge description".  An edge description is one of the following:

+ [src dest]
+ [src dest weight]
+ [src dest attribute-map]
+ an actual edge object

It's up to you to pick an edge description that uniquely identifies your edge.  If your graph contains multiple edges that match a given edge description, ubergraph will arbitrarily pick one such edge for you.  For example, if your multigraph uses different colors to represent several edges between a given pair of nodes, then the edge description `[1 4 {:color :red}]` might be unambiguous for your application.  Using the actual edge object is, of course, the easiest way to ensure you are unqiuely identifying the edge.

Every Loom protocol that takes a simple [src dest] input to descript an edge, in Ubergraph can take one of these richer notions of an edge description.  For example,

```clojure
(weight g [1 4 {:color :red}])  ; what is the weight of the red edge between 1 and 4?
(attr g [:a :b 5] :color)       ; what is the color of the edge from :a to :b with weight 5?
```

#### Edge Queries

In a multigraph, finding all the edges with a given property is of paramount importance.  So Ubergraph, supports the notion of "edge queries".  The most common such query is:

```clojure
(find-edges g :a :b)
```

which means, "find all the edges from :a to :b".

But `find-edges` also supports an arbitrary query-map, which is an attribute map plus, optionally, :src and/or :dest keys to narrow down the search, for example:

```clojure
(find-edges g {:src :a, :dest :b, :weight 5})
(find-edges g {:dest :b, :size :large})
(find-edges g {:color :red})
```

When you don't supply :src and :dest, the query won't be particularly efficient, but at least the capability is there.  `find-edges` returns a sequence of edge objects, which can be used as unambiguous edge descriptions to look up other attributes, remove the edges, etc.

`find-edge` takes the same inputs as `find-edges` but just returns the first match.

### Multidigraphs

Multidigraphs are, as you'd expect, the directed-as-default version of multigraphs.

### Equality

As you build up graphs, random uuids are generated to link edges to attributes.  This randomness means that two graphs which are semantically equal might not necessarily be structurally equal in the Clojure sense.  The hashing and equality for Ubergraphs has been overridden to reflect an intuitive notion of equality, so you can trust, for example that (= (graph [1 2]) (graph [1 2])) even though the two graphs contain different uuids.

### Algorithms

The ubergraph.alg namespace contains a variety of useful graph algorithms. Some of these algorithms are lifted directly from Loom -- because Ubergraph implements Loom's protocols, many of Loom's algorithms work seamlessly on Ubergraphs.  Some of the algorithms that come directly from Loom include: connected-components, connected?, pre-traverse, pre-span, post-traverse, topsort, bf-span, dag?, scc, strongly-connected?, connect, bipartite-color, bipartite?, bipartite-sets.  Some algorithms were adapted from Loom for Ubergraphs with only very minor tweaks, such as loners, distinct-edges, and longest-shortest-path.

However, Ubergraph's Multigraphs and Multidigraphs go far beyond Loom's capabilities, allowing for algorithms with parallel edges.  Most of Loom's path-finding algorithms fail to take into account the possibility of parallel edges.  For example, Loom returns its paths as a list of nodes you visit.  This is sufficient when there is at most one edge between a given pair of nodes, but in a multigraph, it is absolutely necessary to identify precisely which edge you are traveling along.

For this reason, the ubergraph.alg namespace implements a brand-new shortest-path algorithm that is rich with functionality.  Ubergraph's shortest-path algorithm returns paths, which implement a protocol (also found in ubergraph.alg) that allows you get to the following information about a path: edges-in-path, nodes-in-path, cost-of-path (with respect to the kind of search that generated the path), start-of-path, and end-of-path.

To appreciate the power of Ubergraph's shortest-path function, let's take a look at several examples.  But first, for reference, here is the docstring of shortest-path:

```
Finds the shortest path in graph g. You must specify a start node or a collection
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
```

OK, that's a bit of a doozy, but it helps give an idea of how much this one function can do.

For our first running example, let's consider this map of the fictitious country of Altopia, serviced by three airlines.



### API

See the [Codox-generated docs for the Ubergraph API](http://engelberg.github.io/ubergraph/ubergraph.core.html)

## Relationship to Loom

[Loom](https://github.com/aysylu/loom) is currently the most important graph library in the Clojure ecosystem.  Loom strives to achieve three goals:

1. Loom factors graph behavior into a series of Clojure protocols.
2. Loom provides default graph implementations for each of these protocols.
3. Loom provides a library of algorithms that can operate on any graph that implements the relevant abstractions for that algorithm.

Graphs are an important part of my work, and I was disappointed when I discovered that Loom couldn't do many of the things I wanted it to do.  For example, I couldn't mix directed and undirected edges, I couldn't have multiple edges between a given pair of nodes, and I couldn't update weights associated with a given edge or have multiple notions of "cost" associated with a given edge.

My first thought was, "Hey, I'll just make an implementation of multi-edge graphs and submit it to Loom."  But as I started looking carefully at Loom's protocols and algorithms, I realized things weren't quite that simple:

1. Many of Loom's protocols were hard-coded to the assumption that graphs had only a single edge between a pair of nodes.
2. Many of Loom's algorithms were hard-coded to the assumption that graphs had only a single edge between a pair of nodes.

In other words, the protocols didn't even support the behavior necessary to implement a multi-edge graph, and even if the protocols were enhanced, very few of the algorithms would actually work on multi-edge graphs -- too many assumptions were sprinkled throughout the code base that [src dest] vectors are enough to uniquely identify a given edge.

I decided to embark on a five-step plan to improve Loom to cover multi-edge and other complex graph needs.

### Step 1

Submit pull request to Loom to support an Edge abstraction, and to update the built-in protocol functions to accept and return things that support that abstraction.  Step completed, and pull request has been accepted to Loom (Thanks Aysylu!)

### Step 2

Rewrite Loom's algorithms to use this Edge abstraction.  If this is done, then the algorithms will work properly on both single-edge and multi-edge graphs.  Unfortnately, I can't keep up with this on my own -- people keep contributing new "broken" algorithms (i.e., algorithms which assume a single edge between nodes and ignore the new Edge abstraction) to Loom faster than I can possibly fix them.  I'm going to need help from the community for this, so...

### Step 3

Write ubergraph as a concrete implementation of all the graph behvior that Loom currently supports, as well as all the behavior I'd like it to support.  That's a big part of what this library is all about.  Ubergraph provides the authors of Loom's algorithms with a rich graph data structure that has a very different internal structure than Loom's -- offering a way to test the algorithms to ensure they really are programmed to the correct protocol abstractions, and not some underlying notion of Loom's concrete implementation.

### Step 4

Establish an ubergraph.alg namespace where the community can help "curate" Loom's algorithms, identifying algorithms from loom.alg that work out of the box on ubergraphs, modifying algorithms from loom.alg that need modification, and writing new algorithms that leverage ubergraph's added capabilities.

### Step 5

Ideally, I'm hoping that once all of Loom's algorithms have been successfully converted to work with multi-edge graphs, Ubergraph can be merged into Loom as the default multi-edge graph implementation.  If that turns out not to be practical, ubergraph will continue to exist independently as a kind of "Loom+", a graph data structure that works with all of Loom's protocols and algorithms, as well as supporting additional functionality.


## License

Copyright (C) 2014 Mark Engelberg (mark.engelberg@gmail.com)

Distributed under the [Eclipse Public License](http://opensource.org/licenses/eclipse-1.0.php), the same as Clojure.
