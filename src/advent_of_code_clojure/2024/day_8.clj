(ns advent-of-code-clojure.2024.day-8
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils :as grid]
            [advent-of-code-clojure.grid-utils-2 :as grid-2]))

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

(time (solve input))
(time (solve input :repeat? true))

;; grid utils 2
(defn grid-2-apply-translation [grid-map start-coords translation repeat?]
  (loop [antinodes (list) current-coords start-coords]
    (let [next-coords (grid-2/apply-translation translation current-coords)]
      (cond
        (nil? (grid-2/get-item-at-coords grid-map next-coords)) antinodes
        (not repeat?) (cons next-coords antinodes)
        :else (recur (cons next-coords antinodes) next-coords)))))

(defn grid-2-calculate-antinodes [grid-map coords-1 coords-2 repeat?]
  (let [start-nodes (if repeat? [coords-1 coords-2] [])]
    (-> start-nodes
        (into (grid-2-apply-translation grid-map coords-1 (grid-2/get-coord->coord-translation coords-2 coords-1) repeat?))
        (into (grid-2-apply-translation grid-map coords-2 (grid-2/get-coord->coord-translation coords-1 coords-2) repeat?)))))

(defn grid-2-find-antinodes [grid-map coords repeat?]
  (loop [all-antinodes (list) [next-coords & remaining-coords] coords]
    (if (nil? remaining-coords)
      all-antinodes
      (let [new-antinodes (mapcat (fn [coords] (grid-2-calculate-antinodes grid-map next-coords coords repeat?)) remaining-coords)]
        (recur (into all-antinodes new-antinodes) remaining-coords)))))

(defn grid-2-solve [input & {:keys [repeat?] :as _opts}]
  (let [grid-map (grid-2/->grid-map input :xform str)
        grouped-vals (-> grid-map (grid-2/group-coords-by-item) (dissoc ".") vals)]
    (->>  grouped-vals
          (reduce (fn [all-antinodes coords]
                    (into all-antinodes (grid-2-find-antinodes grid-map coords repeat?))) #{})
          count)))

(time (grid-2-solve input))
(time (grid-2-solve input :repeat? true))