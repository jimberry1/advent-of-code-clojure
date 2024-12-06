(ns advent-of-code-clojure.2024.day-4
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils :as grid]))

(def example-input (utils/load-example-input 2024 4))
(def input (utils/load-input-file 2024 4))

(defn prepare-input [input]
  (-> input
      (utils/format-input :line-re-split #"")
      grid/indexed-grid-map))

(defn x->xmas-counter [grid-map directions x-grid-item]
  (reduce (fn [acc direction]
            (let [grid-items-in-direction (grid/walk-grid grid-map direction x-grid-item :steps 3)
                  spells-xmas? (->> grid-items-in-direction (map :val) (apply str) (= "XMAS"))]
              (if spells-xmas? (inc acc) acc)))
          0 directions))

(defn part-1 [puzzle-input]
  (let [grid-map (prepare-input puzzle-input)
        x-locations (->> grid-map grid/grid-map->grid-coll (filter #(= "X" (:val %))))
        allowed-directions (keys grid/all-directions)]
    (reduce (fn [acc x-location]
              (let [num-of-possible-xmas-formations (x->xmas-counter grid-map allowed-directions x-location)]
                (+ acc num-of-possible-xmas-formations)))
            0 x-locations)))

(part-1 input)

(defn is-A-grid-item-valid? [grid-map grid-item]
  (let [allowed-directions [[:up-right :down-left] [:up-left :down-right]]]
    (reduce (fn [acc [direction-1 direction-2]]
              (let [{val-1 :val} (grid/get-value-in-direction grid-map direction-1 grid-item)
                    {val-2 :val} (grid/get-value-in-direction grid-map direction-2 grid-item)
                    is-valid? (or (and (= "M" val-1) (= "S" val-2))
                                  (and (= "S" val-1) (= "M" val-2)))]
                (and acc is-valid?)))
            true allowed-directions)))

;; part 2
(let [grid-map (prepare-input input)]
  (->> grid-map
       grid/grid-map->grid-coll
       (filter #(= "A" (:val %)))
       (filter (partial is-A-grid-item-valid? grid-map))
       count))