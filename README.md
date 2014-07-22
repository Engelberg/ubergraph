# Ubergraph

Ubergraph is a versatile, general-purpose graph data structure for Clojure.

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

	[ubergraph "0.0.1"]

Require ubergraph in your namespace header:

	(ns example.core
	  (:require [ubergraph.core :as uber]))

Ubergraph lists loom as a dependency, so when you add ubergraph to your project, leiningen will also download loom.  This means all of loom's namespaces are also available to you in your program.  Loom is currently organized in a way that its protocols are split across a couple different namespaces.  As a convenience, ubergraph.core provides access to all of loom's protocol functions through its own namespace.

For example, rather than calling `loom.graph/out-edges` and `loom.attr/add-attr`, you can just call `uber/out-edges` and `uber/add-attr`.

## Relationship to Loom

Loom is currently the most important graph library in the Clojure ecosystem.  Loom strives to achieve three goals:

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
