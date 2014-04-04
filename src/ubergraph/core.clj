(ns ubergraph.core)

(defrecord Ubergraph [node-map edge-map allow-parallel? undirected?])

; node-map is a {node-id node}
; edge-map is a {edge-id edge}
; node is a {:id node-id :out-edges {dest-id #{edge-ids}} :in-edges {src-id #{edge-ids}} 
;            :in-degree number :out-degree number :attrs attribute-map}
; edge is a {:id edge-id :src node-id :dest node-id :attrs attribute-map}

(defrecord Node [id out-edges in-edges attrs])
(defrecord Edge [id src dest attrs])

(def node-map :node-map)
(def edge-map :edge-map)

(defn node-id-seq [g] (keys (node-map g)))
(defn node-seq [g] (vals (node-map g)))
(defn edge-id-seq [g] (keys (edge-map g)))
(defn edge-seq [g] (vals (edge-map g)))

(defn get-node [g node-id] ((node-map g) node-id))
(defn get-edge [g edge-id] ((edge-map g) edge-id))

(def id :id)

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
  [g node-id k] (get-in g [:node-map node-id :attrs]))

(defn assoc-node-attr
  [g node-id k v] (assoc-in g [:node-map node-id :attrs k] v))

(defn dissoc-node-attr
  [g node-id k] (update-in g [:node-map node-id :attrs] dissoc k))
  

(defn get-edge-attr
  [g edge-id k] (get-in g [:edge-map edge-id :attrs k]))

(defn get-edge-attrs
  [g edge-id k] (get-in g [:edge-map edge-id :attrs]))
  
(defn assoc-edge-attr
  [g edge-id k v] (assoc-in g [:edge-map edge-id :attrs k] v))

(defn dissoc-edge-attr
  [g edge-id k] (update-in g [:edge-map edge-id :attrs] dissoc k))


  


