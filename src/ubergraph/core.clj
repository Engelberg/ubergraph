(ns ubergraph.core
  (:require [ubergraph.protocols :as up]))

;; Concrete implemenation of ubergraph.protocols

(declare node-seq edge-seq node-info)

(defrecord Ubergraph [node-map allow-parallel? undirected? max-edge-id attrs reverse-edges]
  up/Graph
  (nodes [g] (keys (:node-map g)))
  (edges [g] (for [[node node-info] (:node-map g)
                   edge (:out-edges node-info),
                   :when (not (:duplicate edge))]
               edge))
  (edges [g n1 n2] (get-in g [:node-map n1 :out-edges n2]))
  (has-node? [g node] (boolean (get-in g [:node-map node])))
  (has-edge? [g n1 n2] (boolean (seq (up/edges g n1 n2))))
  (successors [g] (partial up/successors g))
  (successors [g node] (map up/dest (up/out-edges g node)))
  (out-degree [g node] (get-in g [:node-map node :out-degree]))
  (out-edges [g] (partial up/out-edges g)) 
  (out-edges [g node] (apply concat (vals (get-in g [:node-map node :out-edges])))))
; A node-id is anything the user wants it to be -- a number, a keyword, a data structure
; An edge is something with a src, a dest, and an id that can be used to look up attributes

; node-map is a {node-id node-info}
; node-info is a {:out-edges {dest-id #{edge}} :in-edges {src-id #{edge}} 
;                 :in-degree number :out-degree number}
; edge is either Edge or UndirectedEdge

(defrecord NodeInfo [out-edges in-edges out-degree in-degree])
(defrecord UbergraphEdge [id src dest])
(defrecord UbergraphUndirectedEdge [id src dest duplicate])

(defn edge? [o] (or (instance? UbergraphEdge o) (instance? UbergraphUndirectedEdge o)))
(defn undirected-edge? [o] (instance? UbergraphUndirectedEdge o))

(defn node-seq [g] (keys (:node-map g)))
(defn edge-seq
  "Return a sequence of all edges, undirected edges appear only once"
  [g] (for [node (node-seq g),
            edge (:out-edges node),
            :when (not (:duplicate edge))]
        edge))

; A node or edge can always be substituted for a node-id or edge-id, respectively,
; but if you know you have a node or edge, sometimes you can get the information
; you need without going through the graph.

(defn node-info [g node]
  (get-in g [:node-map node]))
  




(defn out-edges
  ([g node-id] (out-edges (get-node g node-id)))
  ([node] (apply concat (vals (:out-edges node)))))

(defn in-edges
  ([g node-id] (in-edges (get-node g node-id)))
  ([node] (apply concat (vals (:in-edges node)))))

(defn src
  ([g edge-id] (src (get-edge g edge-id)))
  ([edge] (:src edge)))

(defn dest
  ([g edge-id] (dest (get-edge g edge-id)))
  ([edge] (:dest edge)))


(defn get-node-attr
  [g node-id k] (get-in g [:node-map node-id :attrs k]))

(defn get-node-attrs
  [g node-id] (get-in g [:node-map node-id :attrs]))

(defn assoc-node-attr
  [g node-id k v] (assoc-in g [:node-map node-id :attrs k] v))

(defn dissoc-node-attr
  [g node-id k] (update-in g [:node-map node-id :attrs] dissoc k))
  

(defn get-edge-attr
  [g edge-id k] (get-in g [:edge-map edge-id :attrs k]))

(defn get-edge-attrs
  [g edge-id] (get-in g [:edge-map edge-id :attrs]))
  
(defn assoc-edge-attr
  [g edge-id k v] (assoc-in g [:edge-map edge-id :attrs k] v))

(defn dissoc-edge-attr
  [g edge-id k] (update-in g [:edge-map edge-id :attrs] dissoc k))


(defn attr
  [g object-id k] (get-in (get-object g object-id) [:attrs k]))

(defn attrs
  [g object-id] (get (get-object g object-id) :attrs))

(defn assoc-attr
  [g object-id k v] 
  (let [object-id (id object-id)
        map-type (if (edge-id? object-id) :edge-map :node-map)]
    (assoc-in g [map-type object-id :attrs k] v)))

(defn dissoc-attr
  [g object-id k] 
  (let [object-id (id object-id),
        map-type (if (edge-id? object-id) :edge-map :node-map)]
    (update-in g [map-type object-id :attrs] dissoc k)))


(defn add-node
  [g node-id]
  (cond
    (get-node g node-id) g  ; node already exists
    (node? node-id) (assoc-in g [:node-map (:id node-id)] node-id)
    :else (assoc-in g [:node-map node-id] (->Node node-id {} {} {}))))

(defn remove-node
  [g node-id]
  (update-in g [:node-map] dissoc (id node-id)))


(defn new-edge-id []
  (keyword "ubergraph.edge" (str (gensym "id"))))

(def fconj (fnil conj #{}))
(def finc (fnil inc 0))

(defn submap? [m1 m2]
  (every? identity (for [[k v] m1] (= (m2 k) v))))

(defn find-edges 
  ([g src dest]
    (get-in g [:node-map src :out-edges dest]))
  ([g {src :src dest :dest :as attributes}]
    (let [edge-ids
          (cond 
            (and src dest) (get-in g [:node-map src :out-edges dest])
            src (out-edges src)
            dest (in-edges dest)
            :else (edge-id-seq g))
          attributes (dissoc attributes :src :dest)]
      (for [edge-id edge-ids
            :when (submap? attributes (get-in g [:edge-map edge-id]))]
        edge-id))))
          
(defn find-edge [& args]
  (first (apply find-edges args)))

(defn add-edge
  ([g src dest] (add-edge g src dest {}))
  ([g src dest attributes]
    (let [g (-> g (add-node src) (add-node dest))
          src (id src), dest (id dest)]
      (cond
        (and (not (:allow-parallel? g)) (find-edge g src dest))
        (update-in g [:edge-map (:id (find-edge g src dest)) :attrs]
                   merge attributes)
    
        (:undirected? g)
        (let [forward-edge-id (new-edge-id),
              backward-edge-id (new-edge-id),
              forward-edge (->UndirectedEdge forward-edge-id src dest attributes
                                             backward-edge-id true)
              backward-edge (->UndirectedEdge backward-edge-id src dest attributes
                                              forward-edge-id false)]
          (-> g
            (assoc-in [:edge-map forward-edge-id] forward-edge)
            (assoc-in [:edge-map backward-edge-id] backward-edge)
            (update-in [:node-map src :out-edges dest] fconj forward-edge-id)
            (update-in [:node-map src :in-edges dest] fconj backward-edge-id)
            (update-in [:node-map src :in-degree] finc)
            (update-in [:node-map src :out-degree] finc)
            (update-in [:node-map dest :out-edges src] fconj backward-edge-id)
            (update-in [:node-map dest :in-edges src] fconj forward-edge-id)
            (update-in [:node-map dest :in-degree] finc)
            (update-in [:node-map dest :out-degree] finc)))
    
        :else
        (let [edge-id (new-edge-id),
              edge (->Edge edge-id src dest attributes)]
          (-> g
            (assoc-in [:edge-map edge-id] edge)
            (update-in [:node-map src :out-edges dest] fconj edge-id)
            (update-in [:node-map dest :in-edges src] fconj edge-id)
            (update-in [:node-map src :out-degree] finc)
            (update-in [:node-map dest :in-degree] finc)))))))

(defn remove-edge
  [g edge-id]
  (let [edge-id (id edge-id),
        edge (get-edge g edge-id)
        src (:src edge)
        dest (:dest edge)]
    (if (nil? edge) g
      (if-let
        [other-direction (:undirected edge)]
        (-> g
          (update-in [:edge-map] dissoc edge-id)
          (update-in [:edge-map] dissoc other-direction)
          (update-in [:node-map src :out-edges dest] disj edge-id)
          (update-in [:node-map src :in-edges dest] disj other-direction)
          (update-in [:node-map src :in-degree] dec)
          (update-in [:node-map src :out-degree] dec)
          (update-in [:node-map dest :out-edges src] disj other-direction)
          (update-in [:node-map dest :in-edges src] disj edge-id)
          (update-in [:node-map dest :in-degree] dec)
          (update-in [:node-map dest :out-degree] dec))
        (-> g
          (update-in [:edge-map] dissoc edge-id)
          (update-in [:node-map src :out-edges dest] disj edge-id)
          (update-in [:node-map dest :in-edges src] disj edge-id)
          (update-in [:node-map src :out-degree] dec)
          (update-in [:node-map dest :in-degree] dec))))))
                  
    
         



