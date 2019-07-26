(ns ubergraph.invariants
  (:require [ubergraph.core :as uber]))


;; This code mostly does its own "independent" checking of the
;; internal data structures within an ubergraph, but it does call
;; several functions from the ubergraph.core namespace to help it do
;; so.  The current ones are listed below, and they either have a
;; pretty trivial implementation, or I have checked the less trivial
;; ones carefully.

;; nodes
;; has-node?
;; edges
;; edge?
;; mirror-edge?
;; undirected-edge?
;; src
;; dest
;; ubergraph?


(defn nodes-also-edges [g]
  (filter uber/edge? (uber/nodes g)))


(defn edge-id-set [g]
  (set (map :id (uber/edges g))))


(defn nodes-also-edge-uuids [g]
  (filter (edge-id-set g) (uber/nodes g)))


(defn nodes-logical-false [g]
  (filter #(false? (boolean %)) (uber/nodes g)))


(defn bad-attrs-keys [g]
  (let [edge-id-set (edge-id-set g)]
    (remove #(or (contains? edge-id-set %)
                 (contains? (:node-map g) %))
            (keys (:attrs g)))))


(defn attrs-val-not-map [g]
  (remove map? (vals (:attrs g))))


(defn node-map-val-wrong-type-or-keys [g]
  (remove (fn [x]
            (and (instance? ubergraph.core.NodeInfo x)
                 (= #{:in-edges :out-edges :in-degree :out-degree}
                    (set (keys x)))))
          (vals (:node-map g))))


(defn node-info-wrong-degree [g]
  (remove (fn [i]
            (and
             (= (:in-degree i) (reduce + (map count (vals (:in-edges i)))))
             (= (:out-degree i) (reduce + (map count (vals (:out-edges i)))))))
          (vals (:node-map g))))


;; Every NodeInfo record has a map as the value associated with its
;; keys :in-edges and :out-edges.  Every such map has keys that are
;; all node values in the graph.

(defn node-info-with-non-node-key [g]
  (let [all-keys-are-nodes? (fn [m]
                              (every? #(uber/has-node? g %) (keys m)))]
    (remove (fn [i]
              (and (all-keys-are-nodes? (:in-edges i))
                   (all-keys-are-nodes? (:out-edges i))))
            (vals (:node-map g)))))


;; Following up on the comment of the previous function above, the
;; associated values must be *non-empty* Clojure sets of edge objects.
;; Maintaining the non-empty part of this invariant helps to enable
;; faster implementations of the functions successors and
;; predecessors.

(defn node-info-with-wrong-val [g]
  (let [good-edge-set? (fn [x]
                         (and (set? x)
                              (not (zero? (count x)))
                              (every? uber/edge? x)))]
    (remove (fn [i]
              (and (every? good-edge-set? (vals (:in-edges i)))
                   (every? good-edge-set? (vals (:out-edges i)))))
            (vals (:node-map g)))))


(defn duplicates [coll]
  (into {} (for [[k v] (frequencies coll)
                 :when (> v 1)]
             [k v])))

(comment
(= {} (duplicates [1 2 3 4]))
(= {1 2, 3 3} (duplicates [1 2 3 4 1 3 5 3]))
)


(defn duplicate-edge-uuids [g]
  (->> (uber/edges g)
       (remove uber/mirror-edge?)
       (map :id)
       duplicates))


;; Correct undirected edges come in pairs, one with :mirror? true, the
;; other false, both with the same id, and with src and dest swapped.
;; Return any undirected edges found that do not meet these
;; conditions.
(defn undirected-edges-paired-incorrectly [g]
  (let [uedges-by-id (->> (uber/edges g)
                          (filter uber/undirected-edge?)
                          (group-by :id))]
    (remove (fn good-uedge-pair [[uuid edges]]
              (and (= 2 (count edges))
                   (= #{true false} (set (map uber/mirror-edge? edges)))
                   (let [[e1 e2] edges]
                     (and (= (uber/src e1) (uber/dest e2))
                          (= (uber/dest e1) (uber/src e2))))))
            uedges-by-id)))


(defn edges-with-wrong-src-or-dest [g]
  (concat
   (for [[n1 node-info] (:node-map g)
         [n2 out-edges] (:out-edges node-info)
         out-edge out-edges
         :when (or (not= n1 (uber/src out-edge))
                   (not= n2 (uber/dest out-edge)))]
     [n1 n2 out-edge])
   (for [[n1 node-info] (:node-map g)
         [n2 in-edges] (:in-edges node-info)
         in-edge in-edges
         :when (or (not= n2 (uber/src in-edge))
                   (not= n1 (uber/dest in-edge)))]
     [n1 n2 in-edge])))


(defn check-invariants
  "Checks whether the internal data structures of an ubergraph satisfy
  invariants assumed by the implementation.  The implementation of
  this function is one way to document these invariants, plus being an
  executable way to check for bugs in the implementation.

  Returns a map with the key :error, where the associated value is a
  boolean indicating whether any errors were found.  Other keys can be
  returned if there is an error, indicating details about any
  invariant violations found."
  [g]
  (if-not (instance? ubergraph.core.Ubergraph g)
    {:error true
     :description "Not an ubergraph"}

    (let [{:keys [node-map allow-parallel? undirected? attrs cached-hash]} g]
      (cond
        (not (map? node-map))
        {:error true
         :description "node-map value is not a map"}

        (not (boolean? allow-parallel?))
        {:error true
         :description "allow-parallel? value is not a boolean"}

        (not (boolean? undirected?))
        {:error true
         :description "undirected? value is not a boolean"}

        (not (map? attrs))
        {:error true
         :description "attrs value is not a map"}

        (not (instance? clojure.lang.Atom cached-hash))
        {:error true
         :description "cached-hash value is not an atom"}

        (seq (nodes-also-edges g))
        {:error true
         :description "At least one node is also an edge"
         :data (first (nodes-also-edges g))}

        (seq (nodes-also-edge-uuids g))
        {:error true
         :description "At least one node is also an edge uuid value"
         :data (first (nodes-also-edge-uuids g))}

        (seq (bad-attrs-keys g))
        {:error true
         :description "At least one key in :attrs is neither a node nor an edge id"
         :data (first (bad-attrs-keys g))}

        (seq (attrs-val-not-map g))
        {:error true
         :description "At least one value in :attrs map is not a map"
         :data (first (attrs-val-not-map g))}
        
        (seq (node-map-val-wrong-type-or-keys g))
        {:error true
         :description "At least one value in :node-map is not a NodeInfo record, or has the wrong keys"
         :data (first (node-map-val-wrong-type-or-keys g))}
        
        (seq (node-info-wrong-degree g))
        {:error true
         :description "At least one value in :node-map has wrong value for :in-degree or :out-degree"
         :data (first (node-info-wrong-degree g))}

        (seq (node-info-with-non-node-key g))
        {:error true
         :description "At least one value in :node-map has a key that is not a node"
         :data (first (node-info-with-non-node-key g))}

        (seq (node-info-with-wrong-val g))
        {:error true
         :description "At least one value in :node-map has a value that is not a non-empty set of edges"
         :data (first (node-info-with-wrong-val g))}

        (seq (undirected-edges-paired-incorrectly g))
        {:error true
         :description "At least one undirected edge not paired correctly"
         :data (first (undirected-edges-paired-incorrectly g))}

        ;; When ignoring edges with :mirror? equal to true, all edge
        ;; ids should be unique.
        (seq (duplicate-edge-uuids g))
        {:error true
         :description "At least one duplicate edge UUID"
         :data (first (duplicate-edge-uuids g))}

        ;; TBD: if allow-parallel? is false, then there are no
        ;; parallel edges in the graph.

        (seq (edges-with-wrong-src-or-dest g))
        {:error true
         :description "At least one edge has the wrong src or dest node"
         :data (first (edges-with-wrong-src-or-dest g))}

        ;; Do NOT create a check that if a graph has undirected? true,
        ;; then all of its edges are undirected, or conversely that if
        ;; a graph has undireted? false then all of its edges are
        ;; directed.  There are published functions add-directed-edges
        ;; and add-undirected-edges that enable adding the "other
        ;; kind" of edge to a graph that was created as directed, or
        ;; undirected.

        ;; Node values of false or nil lead to bugs in many functions
        ;; of Loom and Ubergraph.  Multiple functions use nil-punning,
        ;; or create a set of nodes like 'some-set', and then do a
        ;; membership check via evaluating an expression
        ;; like (some-set possible-node), and assume the result is
        ;; logical true if the node is in the set.
        (seq (nodes-logical-false g))
        {:error true
         :description "At least one node is a logical false value"
         :data (nodes-logical-false g)}
        
        :else
        {:error false}))))
