(ns ubergraph.viz
  (:require [dorothy.core :as d]
            [ubergraph.core :as u]))

(defn viz-graph
  "Uses graphviz to generate a visualization of your graph. Graphviz 
must be installed on your computer and in your path. Passes along
to graphviz the attributes on the nodes and edges, so graphviz-related
attributes such as color, style, label, etc. will be respected.

Takes an optional map which can contain:
:layout :dot, :neato, :fdp, :sfdp, :twopi, or :circo
:save {:filename _, :format _} where format is one of
  :bmp :eps :gif :ico :jpg :jpeg :pdf :png :ps :ps2 :svgz :tif :tiff :vmlz :wbmp"
  ([g] (viz-graph g {}))
  ([g {layout :layout {filename :filename format :format :as save} :save 
       :or {layout :dot}}]
    (let [ns (u/nodes g),
          es (u/edges g)
          nodes (for [n ns]
                  [(if (or (string? n)
                           (keyword? n)
                           (number? n))
                     n
                     (str n))
                   (u/attrs g n)]),
          directed-edges (for [e es :when (u/directed-edge? e)]
                           [(u/src e) (u/dest e) (u/attrs g e)])
          undirected-edges (for [e es :when (and (u/undirected-edge? e)
                                                 (not (u/mirror-edge? e)))]
                             [(u/src e) (u/dest e)
                              (merge {:dir :none} (u/attrs g e))])]
      (-> (concat [{:layout layout}] 
                  nodes directed-edges undirected-edges)
        d/digraph
        d/dot
        (cond->
          save (d/save! filename {:format format})          
          (not save) d/show!)))))

