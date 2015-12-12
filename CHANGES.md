# Ubergraph Change Log

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

