(ns ubergraph.core
  (:require [ubergraph.protocols :as up]))

;; Concrete implemenation of ubergraph.protocols

; These are the functions that are too lengthy to define inline
; in the Ubergraph record.
(declare transpose get-edge find-edges add-node add-edge remove-node remove-edge)

(defrecord Ubergraph [node-map allow-parallel? undirected? max-edge-id attrs reverse-edges]
  up/Graph
  (nodes [g] (keys (:node-map g)))
  (edges [g] (for [[node node-info] (:node-map g)
                   [dest edges] (:out-edges node-info),
                   edge edges
                   :when (not (:duplicate edge))]
               edge))
  (edges [g n1 n2] (get-in g [:node-map n1 :out-edges n2]))
  (has-node? [g node] (boolean (get-in g [:node-map node])))
  (has-edge? [g n1 n2] (boolean (seq (up/edges g n1 n2))))
  (successors [g] (partial up/successors g))
  (successors [g node] (map up/dest (up/out-edges g node)))
  (out-degree [g node] (get-in g [:node-map node :out-degree]))
  (out-edges [g] (partial up/out-edges g)) 
  (out-edges [g node] (apply concat (vals (get-in g [:node-map node :out-edges]))))
  
  up/Digraph
  (predecessors [g] (partial up/predecessors g))
  (predecessors [g node] (map up/src (up/in-edges g node)))
  (in-degree [g node] (get-in g [:node-map node :in-degree]))
  (in-edges [g] (partial up/in-edges g))
  (in-edges [g node] (apply concat (vals (get-in g [:node-map node :in-edges]))))
  (transpose [g] (transpose g))
  
  up/WeightedGraph
  ; Ubergraphs by default store weight in an attribute :weight
  ; Using an attribute allows us to modify the weight with the AttrGraph protocol
  (weight [g] (partial up/weight g))
  (weight [g e] (get-in g [:attrs e :weight]))
  (weight [g n1 n2] (get-in g [:attrs (get-edge g n1 n2) :weight]))
  
  up/EditableGraph
  (add-nodes* [g nodes] (reduce add-node g nodes))
  ; edge definition should be [src dest] or [src dest weight] or [src dest attribute-map]
  (add-edges* [g edge-definitions] (reduce (fn [g edge] (apply add-edge g edge)) edge-definitions)) 
  (remove-nodes* [g nodes] (reduce remove-node g nodes))
  (remove-edges* [g edges] (reduce remove-edge g edges))
  (remove-all [g] (Ubergraph. {} allow-parallel? undirected? 0 {} {}))
  
  up/AttrGraph
  (add-attr [g node-or-edge k v] (assoc-in g [:attrs node-or-edge k] v))
  (add-attr [g n1 n2 k v] (up/add-attr g (get-edge g n1 n2) k v))
  (remove-attr [g node-or-edge k] (update-in g [:attrs node-or-edge] dissoc k))
  (remove-attr [g n1 n2 k] (up/remove-attr g (get-edge g n1 n2) k))
  (attr [g node-or-edge k] (get-in g [:attrs node-or-edge k]))
  (attr [g n1 n2 k] (up/attr g (get-edge g n1 n2) k))
  (attrs [g node-or-edge] (get-in g [:attrs node-or-edge] {}))
  (attrs [g n1 n2] (up/attrs g (get-edge g n1 n2)))
  
  up/UndirectedEdge
  (reverse-edge [g edge] (get-in g [:reverse-edges edge]))
  
  up/Query
  (find-edges [g edge] (find-edges g edge))
  (find-edge [g edge] (first (up/find-edges g edge))))
    
; A node-id is anything the user wants it to be -- a number, a keyword, a data structure
; An edge is something with a src, a dest, and an id that can be used to look up attributes

; node-map is a {node-id node-info}
; node-info is a {:out-edges {dest-id #{edge}} :in-edges {src-id #{edge}} 
;                 :in-degree number :out-degree number}
; edge is either Edge or UndirectedEdge

(defrecord NodeInfo [out-edges in-edges out-degree in-degree])
(defrecord Edge [id src dest]
  up/Edge
  (src [edge] src)
  (dest [edge] dest))
(defrecord UndirectedEdge [id src dest duplicate]
  up/Edge
  (src [edge] src)
  (dest [edge] dest))

(defn- get-edge [g n1 n2] (first (up/edges g n1 n2)))

(defn edge? [o] (or (instance? Edge o) (instance? UndirectedEdge o)))
(defn undirected-edge? [o] (instance? UndirectedEdge o))

(defn- add-node
  [g node]
  (cond
    (get-in g [:node-map node]) g  ; node already exists
    :else (assoc-in g [:node-map node] (->NodeInfo {} {} 0 0))))

(defn- remove-node
  [g node]
  (update-in g [:node-map] dissoc node))

(def fconj (fnil conj #{}))
(def finc (fnil inc 0))

(defn- submap? [m1 m2]
  (every? identity (for [[k v] m1] (= (m2 k) v))))

(defn- find-edges 
  ([g src dest]
    (get-in g [:node-map src :out-edges dest]))
  ([g {src :src dest :dest :as attributes}]
    (let [edges
          (cond 
            (and src dest) (get-in g [:node-map src :out-edges dest])
            src (up/out-edges g src)
            dest (up/in-edges g dest)
            :else (up/edges g))
          attributes (dissoc attributes :src :dest)]
      (for [edge edges
            :when (submap? attributes (get-in g [:attrs edge]))]
        edge))))
          
(defn- find-edge [& args]
  (first (apply find-edges args)))

(defn add-directed-edge [g src dest attributes]
  (let [g (-> g (add-node src) (add-node dest))
        edge-id (inc (:max-edge-id g))
        edge (->Edge edge-id src dest)
        new-attrs (if attributes 
                    (assoc (:attrs g) edge attributes)
                    (:attrs g))
        node-map (:node-map g)
        node-map-src (get node-map src)
        node-map-dest (get node-map dest)              
        new-node-map-src (-> node-map-src             
                           (update-in [:out-edges dest] fconj edge)
                           (update-in [:out-degree] finc))
        new-node-map-dest (-> node-map-dest
                            (update-in [:in-edges src] fconj edge)
                            (update-in [:in-degree] finc))
        new-node-map (assoc node-map src new-node-map-src dest new-node-map-dest)]
    (Ubergraph. new-node-map (:allow-parallel? g) (:undirected? g) edge-id new-attrs (:reverse-edges g))))

(defn add-undirected-edge [g src dest attributes]
  (let [g (-> g (add-node src) (add-node dest))
        max-edge-id (:max-edge-id g)
        forward-edge-id (inc max-edge-id),
        backward-edge-id (+ max-edge-id 2),
        forward-edge (->UndirectedEdge forward-edge-id src dest false),
        backward-edge (->UndirectedEdge backward-edge-id dest src true)
        new-attrs (if attributes 
                    (assoc (:attrs g) forward-edge attributes backward-edge attributes)
                    (:attrs g))
        node-map (:node-map g)
        node-map-src (get node-map src)
        node-map-dest (get node-map dest)              
        new-node-map-src (-> node-map-src             
                           (update-in [:out-edges dest] fconj forward-edge)
                           (update-in [:in-edges dest] fconj backward-edge)
                           (update-in [:in-degree] finc)
                           (update-in [:out-degree] finc))
        new-node-map-dest (-> node-map-dest
                            (update-in [:out-edges src] fconj backward-edge)
                            (update-in [:in-edges src] fconj forward-edge)
                            (update-in [:in-degree] finc)
                            (update-in [:out-degree] finc))
        new-node-map (assoc node-map src new-node-map-src dest new-node-map-dest)
        new-reverse-edges (assoc (:reverse-edges g) forward-edge backward-edge backward-edge forward-edge)]
    (Ubergraph. new-node-map (:allow-parallel? g) (:undirected? g) backward-edge-id new-attrs new-reverse-edges)))

(defn add-edge
  ([g src dest] (add-edge g src dest nil))
  ([g src dest attributes]
    (cond
      (and (not (:allow-parallel? g)) (get-edge g src dest))
      (update-in g [:attrs (get-edge g src dest)]
                 merge attributes)
    
      (:undirected? g) (add-undirected-edge g src dest attributes)
      :else (add-directed-edge g src dest attributes))))
 

(defn- remove-edge
  [g edge]
  ; Check whether edge exists before deleting
  (let [src (:src edge)
        dest (:dest edge)]
    (if (get-in g [:node-map src :out-edges dest edge])
        (if-let
          [reverse-edge (get-in g [:reverse-edges edge])]
          (-> g
            (update-in [:attrs] dissoc edge reverse-edge)
            (update-in [:node-map src :out-edges dest] disj edge)
            (update-in [:node-map src :in-edges dest] disj reverse-edge)
            (update-in [:node-map src :in-degree] dec)
            (update-in [:node-map src :out-degree] dec)
            (update-in [:node-map dest :out-edges src] disj reverse-edge)
            (update-in [:node-map dest :in-edges src] disj edge)
            (update-in [:node-map dest :in-degree] dec)
            (update-in [:node-map dest :out-degree] dec))
          (-> g
            (update-in [:edge-map] dissoc edge)
            (update-in [:node-map src :out-edges dest] disj edge)
            (update-in [:node-map src :out-degree] dec)
            (update-in [:node-map dest :in-edges src] disj edge)
            (update-in [:node-map dest :in-degree] dec)))
        g)))
        
(defn- swap-edge [edge]
  (assoc edge :src (:dest edge) :dest (:src edge)))

(defn- transpose [{:keys [node-map allow-parallel? undirected? max-edge-id attrs reverse-edges]}]
  (let [new-node-map
        (into {} (for [[node {:keys [in-edges out-edges in-degree out-degree]}] node-map
                       :let [new-in-edges (into {} (for [[k v] in-edges] [k (set (map swap-edge v))])),
                             new-out-edges (into {} (for [[k v] out-edges] [k (set (map swap-edge v))]))]]
                   [node (NodeInfo. new-in-edges new-out-edges in-degree out-degree)])),
        
        new-node-info (assoc node-info :in-edges out-edges :out-edges :in-edges),
        
        new-attrs (into {} (for [[o attr] attrs]
                             (if (edge? o) [(swap-edge o) attrs] [o attrs])))]
    
    (Ubergraph. new-node-map allow-parallel? undirected? max-edge-id new-attrs reverse-edges)))
                
(defn build-graph
  "Right now, Ubergraph just supports building graphs using nodes and edge specifications
of the form [src dest], [src dest weight], or [src dest attribute-map]"
  [g & inits]
  (letfn [(build [g init]
           (cond
             ;; edge
             (and (vector? init) (#{2,3} (count init)))
             (if (number? (get init 2))
               (add-edge g (init 0) (init 1) {:weight (init 2)})
               (add-edge g (init 0) (init 1) (get init 2)))             
             ;; node
             :else (add-node g init)))]
    (reduce build g inits)))

(defn multigraph [& inits]
  (apply build-graph (->Ubergraph {} true true 0 {} {}) inits))

(defn multidigraph [& inits]
  (apply build-graph (->Ubergraph {} true false 0 {} {}) inits))

(defn graph [& inits]
  (apply build-graph (->Ubergraph {} false true 0 {} {}) inits))

(defn digraph [& inits]
  (apply build-graph (->Ubergraph {} false false 0 {} {}) inits))


                   
                       
         



