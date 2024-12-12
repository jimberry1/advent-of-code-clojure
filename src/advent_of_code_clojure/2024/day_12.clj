(ns advent-of-code-clojure.2024.day-12
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils-2 :as grid-2]
            [advent-of-code-clojure.node-utils :as node-utils]))

(def example-input (utils/load-example-input 2024 12))
(def input (utils/load-input-file 2024 12))

(defn prepare-input [input]
  (utils/format-input input :line-re-split #"" :input-xform grid-2/->grid-map))

(def possible-translations (vals (select-keys grid-2/all-directions [:up :down :left :right])))

(defn get-valid-neighbours [grid-map tree coordinates]
  (->> coordinates
       (grid-2/apply-translations possible-translations)
       (filter #(= (grid-2/get-item-at-coords grid-map %) tree))))

(defn find-price [tree-set]
  (->> tree-set
       (mapcat #(grid-2/apply-translations possible-translations %))
       (remove tree-set)
       count
       (* (count tree-set))))

(defn grid-map->tree-groups [grid-map]
  (->> grid-map
       grid-2/group-coords-by-item
       (mapcat (fn [[tree all-coordinates-for-tree]]
                 (node-utils/nodes-join-all all-coordinates-for-tree (partial get-valid-neighbours grid-map tree))))))

(defn is-every-neighbour-in-set? [tree-set item]
  (every? tree-set (grid-2/apply-translations possible-translations item)))

(defn get-periphery-items-of-set [tree-set]
  (transduce (remove #(is-every-neighbour-in-set? tree-set %)) conj [] tree-set))

(defn count-sides [tree-set allowed-translations periphery-tree-coordinates]
  (let [outside-neighbours (transduce (comp (mapcat #(grid-2/apply-translations allowed-translations %)) (remove tree-set)) conj #{} periphery-tree-coordinates)]
    (count (node-utils/nodes-join-all outside-neighbours (comp (partial filter outside-neighbours) (partial grid-2/apply-translations possible-translations))))))

(def x-side-translations (vals (select-keys grid-2/all-directions [:left :right])))
(def y-side-translations (vals (select-keys grid-2/all-directions [:up :down])))

(defn find-discounted-price [tree-set]
  (let [all-edge-trees (get-periphery-items-of-set tree-set)
        [tree-columns tree-rows] (->> [first second] (map #(group-by % all-edge-trees)) (map vals))
        vertical-side-count (transduce (map (partial count-sides tree-set x-side-translations)) + tree-columns)
        horizontal-side-count (transduce (map (partial count-sides tree-set y-side-translations)) + tree-rows)]
    (* (count tree-set) (+ vertical-side-count horizontal-side-count))))

(defn solve [puzzle-input & {:keys [apply-discount?]}]
  (->> puzzle-input
       prepare-input
       grid-map->tree-groups
       (map (if apply-discount? find-discounted-price find-price))
       (reduce +)))

(time (solve input))
(time (solve input :apply-discount? true))