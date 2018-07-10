# Ubergraph Change Log

## 0.5.1

### Enhancements

* Added :dot file format option to viz-graph, to write out the file in GraphViz's dot format.

## 0.5.0

### Upgrades

* Upgraded to work with Loom 1.0.1.

## 0.4.0

### Enhancements

* Implemented `ubergraph?` predicate.
* Added `ubergraph->edn` and `edn->ubergraph` as a way to serialize and deserialize ubergraphs.
* Added print-dup multimethod for serialization via print-dup.

## 0.3.1

### Enhancements

* `viz-graph` can now take graph attributes in the opts map.  (Thanks to masztal for this PR).

## 0.3.0

### Breaking change

* Calling `nodes` on a graph now returns a seq, not a keySet.

## 0.2.3

### Bug Fixes

* Fixed bug that caused a nil attribute map to appear when adding the same directed edge twice to a non-multidigraph (via add-directed-edges*), affecting viz-graph.

## 0.2.2

### Bug Fixes

* Fixed bug that caused a nil attribute map to appear when adding the same directed edge twice to a non-multidigraph, affecting viz-graph.

## 0.2.1

### Bug Fixes

* Fixed bug in find-edges with queries containing more than :src and :dest.  Thanks to lomin for finding and providing the pull request for this bug.

## 0.2.0

### Bug Fixes

* Fixed problem with `loop' edges that go from a node to itself.

### Improvements

* Clarified some doc strings, and added note to README that some algorithms require Java 8.
* Added `undirected-graph?`, `allow-parallel-edges?`, `ubergraph`

### Breaking Changes

* Improved equality semantics for undirected graphs so the order in which the src and dest are listed don't affect equality.  (graph [1 2]) and (graph [2 1]) now compare as equal.

## 0.1.9

### Improvements

* Sped up equality testing

## 0.1.8

### Bug Fixes

* Removed reflective call in `nodes`
* Changed graph equality to treat attributes of nil to be the same as attributes of {}
* Changed doc string for `add-edges`
* Fixed merging of attributes when parallel edges are added in a non-multi graph.

## 0.1.7

### Bug Fixes

* Patch from emlyn so ubergraph runs on Clojure 1.8

## 0.1.6

### Bug Fixes

* Patch from emlyn so viz works on nodes that are maps.

## 0.1.5

### Enhancements

* Graph equality now respects node attributes.  Graphs with different node attributes will not be considered equal.

## 0.1.4

### Bug Fixes

* Added clojure.pprint as an explicit require, which helps with certain compilation scenarios.

## 0.1.3

### Bug Fixes

* Fixed printing of attributes that are classes, records, or strings with special characters

## 0.1.2

### Bug Fixes

* least-cost searches didn't work if nodes weren't comparable, e.g., maps

## 0.1.1

### Enhancements

* Added add-nodes-with-attrs which takes [node attr-map] forms.
* Added support for [node attr-map] inits in build-graph and constructors.
* Added support for ^:node and ^:edge metadata in inits, in build-graph and constructors.
* Added add-attrs, remove-attrs, and set-attrs.

## 0.1.0 

* Initial Release

