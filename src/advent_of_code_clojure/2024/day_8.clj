(ns advent-of-code-clojure.2024.day-8
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils :as grid]))

(def example-input (utils/load-example-input 2024 8))
(def input (utils/load-input-file 2024 8))

(defn apply-translation [grid-map operation start-node translation repeat?]
  (loop [nodes (list) current-node start-node]
    (let [next-item (grid/apply-translation-get-value grid-map current-node translation :operation operation)]
      (cond
        (nil? next-item) nodes
        (not repeat?) (cons next-item nodes)
        :else (recur (cons next-item nodes) next-item)))))

(defn calculate-antinodes [grid-map node-1 node-2 repeat?]
  (let [translation (grid/calc-translation node-1 node-2)
        start-nodes (if repeat? [node-1 node-2] [])]
    (-> start-nodes
        (into (apply-translation grid-map - node-1 translation repeat?))
        (into (apply-translation grid-map + node-2 translation repeat?)))))

(defn find-antinodes [grid-map nodes repeat?]
  (loop [antinodes (list) [next-node & remaining-nodes] nodes]
    (if (nil? remaining-nodes)
      antinodes
      (let [new-antinodes (mapcat (fn [node] (calculate-antinodes grid-map next-node node repeat?)) remaining-nodes)]
        (recur (into antinodes new-antinodes) remaining-nodes)))))

(defn solve [input & {:keys [repeat?] :as _opts}]
  (let [grid-map (grid/indexed-grid-map input :xform str)
        grouped-vals (->> grid-map grid/grid-map->grid-coll (remove #(= "." (:val %))) (group-by :val))]
    (->>  grouped-vals
          (reduce-kv (fn [all-antinodes _val nodes-for-val]
                       (into all-antinodes (find-antinodes grid-map nodes-for-val repeat?))) #{})
          count)))

(solve input)
(solve input :repeat? true)