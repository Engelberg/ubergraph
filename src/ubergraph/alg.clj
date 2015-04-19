(ns ubergraph.alg
  (:require [ubergraph.core :as uber])
  (:import java.util.PriorityQueue java.util.HashMap))

;; Shortest path
;; For speed, on Java, use mutable queues and hash maps
;; Consider using priority maps for better portability to Clojurescript

(defn- find-path
  ([to backlinks] (find-path to backlinks ()))
  ([to ^HashMap backlinks path]
    (if-let [prev-edge (.get backlinks to)]
      (recur (uber/src prev-edge) backlinks (cons prev-edge path))
      path)))

;(defn- bf-path [g to ^java.util.LinkedList queue ^HashMap total-distances ^HashMap backlinks]
;  (loop []
;    (if-let [node (.poll queue)]
;      (if (= node to)
;        (find-path to backlinks)
;        (do (when (<= total-cost (.get total-costs node))
;              (doseq [edge (uber/out-edges g node)]
;                (let [dst (uber/dest edge),
;                      cost (weight-fn edge),
;                      new-cost (+ total-cost cost)
;                      previous-cost (.get total-costs dst)]
;                  (when (not (and previous-cost (< previous-cost new-cost)))
;                    (.add queue [new-cost dst])
;                    (.put total-costs dst new-cost)
;                    (.put backlinks dst edge)))))
;          (recur)))  
;      nil)))

(defn- shortest-path-with-weights-helper [g to ^PriorityQueue queue ^HashMap total-costs ^HashMap backlinks 
                                          weight-fn]
  (loop []
    (if-let [[total-cost node] (.poll queue)]
      (if (= node to)
        (find-path to backlinks)
        (do (when (< total-cost (.get total-costs node))
              (doseq [edge (uber/out-edges g node)]
                (let [dst (uber/dest edge),
                      cost (weight-fn edge),
                      new-cost (+ total-cost cost)
                      previous-cost (.get total-costs dst)]
                  (when (not (and previous-cost (< previous-cost new-cost)))
                    (.add queue [new-cost dst])
                    (.put total-costs dst new-cost)
                    (.put backlinks dst edge)))))
          (recur)))  
      nil)))

(defn shortest-path-with-weights [g from to weight-fn]
  (let [cost-map (HashMap.),
        backlink-map (HashMap.)
        queue (PriorityQueue.)]
    (.put cost-map from 0)
    (.put backlink-map from nil)
    (.add queue [0 from])
    (shortest-path-with-weights-helper g to queue cost-map backlink-map weight-fn)))

(defn- shortest-path-with-heuristic-helper [g to ^PriorityQueue queue ^HashMap cost-map ^HashMap backlinks 
                             weight-fn heuristic-fn]
  (loop []
    (if-let [[estimated-total-cost [cost-from-start node]] (.poll queue)]
      (if (= node to)
        (find-path to backlinks)
        (do (when (< cost-from-start (.get cost-map node)))
              (doseq [edge (uber/out-edges g node)]
                (let [dst (uber/dest edge),
                      edge-cost (weight-fn edge),
                      new-cost (+ cost-from-start edge-cost)
                      previous-cost-to-dst (.get cost-map dst)]
                  (when (not (and previous-cost-to-dst (< previous-cost-to-dst new-cost)))
                    (.add queue [(+ new-cost (heuristic-fn dst to)) [new-cost dst]])
                    (.put cost-map dst new-cost)
                    (.put backlinks dst edge))))
              (recur)))  
      nil)))

(defn shortest-path-with-heuristic [g from to weight-fn heuristic-fn]
  (let [cost-map (HashMap.),
        backlink-map (HashMap.)
        queue (PriorityQueue.)]
    (.put cost-map from 0)
    (.put backlink-map from nil)
    (.add queue [(heuristic-fn from to) [0 from]])
    (shortest-path-with-heuristic-helper g to queue cost-map backlink-map weight-fn heuristic-fn)))

