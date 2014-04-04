(ns ubergraph.core)

(defrecord Ubergraph [node-map edge-map allow-parallel? undirected?])

; node-map is a {node-id node}
; edge-map is a {edge-id edge}
; node is a {:id node-id :out-edges {dest-id #{edge-ids}} :in-edges {src-id #{edge-ids}} 
;            :in-degree number :out-degree number :attrs attribute-map}
; edge is a {:id edge-id :src node-id :dest node-id :attrs attribute-map}

; edge-ids are keywords in the namespace ubergraph.edge
; node-ids are anything else

(defn edge-id? [o] (and (keyword? o) (= "ubergraph.edge" (namespace o))))
(defn node-id? [o] (not (edge-id? o)))

(defrecord Node [id out-edges in-edges attrs])
(defrecord Edge [id src dest attrs])

(defn node? [o] (instance? Node o))
(defn edge? [o] (instance? Edge o))

; An undirected edge stores a link to its counterpart edge in undirected
; and a forward field which will be marked as true for one of the directions
; and false for the other direction.
(defrecord UndirectedEdge [id src dest attrs undirected forward]) 
(defn undirected-edge? [o] (instance? UndirectedEdge o))

(defn node-id-seq [g] (keys (:node-map g)))
(defn node-seq [g] (vals (:node-map g)))
(defn edge-id-seq [g] (keys (:edge-map g)))
(defn edge-seq [g] (vals (:edge-map g)))

; A node or edge can always be substituted for a node-id or edge-id, respectively,
; but if you know you have a node or edge, sometimes you can get the information
; you need without going through the graph.

(defn get-node [g node-id] 
  (if (node? node-id) node-id ((:node-map g) node-id)))
(defn get-edge [g edge-id] 
  (if (edge? edge-id) edge-id ((:edge-map g) edge-id)))
(defn get-object [g id]
  (cond
    (or (node? id) (edge? id)) id
    (edge-id? id) ((:edge-map g) id)
    :else ((:node-map g) id)))

(defn id [o]
  (cond
    (or (node? o) (edge? o) (edge-id? o)) (:id o)
    :else o))

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
                  
    
         



