(ns advent-of-code-clojure.2024.day-4
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 4))
(def input (utils/load-input-file 2024 4))

(defn prepare-input [input]
  (-> input
      (utils/format-input :line-re-split #"")
      utils/indexed-grid-map))

(defn x->xmas-counter [grid-map translations x-grid-item]
  (reduce (fn [acc direction]
            (let [is-valid? (->> (utils/walk-grid grid-map direction x-grid-item :steps 3)
                                 (map :val)
                                 (apply str)
                                 (= "XMAS"))]
              (if is-valid? (inc acc) acc)))
          0 translations))

(defn part-1 [puzzle-input]
  (let [grid-map (prepare-input puzzle-input)
        x-locations (->> grid-map
                         utils/grid-map->grid-coll
                         (filter #(= "X" (:val %))))
        allowed-translations (keys utils/all-translations)]
    (reduce (fn [acc x-location]
              (let [num-of-possible-xmas-formations (x->xmas-counter grid-map allowed-translations x-location)]
                (+ acc num-of-possible-xmas-formations)))
            0 x-locations)))

(part-1 input)

(defn is-a-valid? [grid-map grid-item]
  (let [allowed-translations [[:up-r :down-l] [:up-l :down-r]]]
    (reduce (fn [acc [trans-1 trans-2]]
              (let [{val-1 :val} (utils/apply-translation-get-val grid-map trans-1 grid-item)
                    {val-2 :val} (utils/apply-translation-get-val grid-map trans-2 grid-item)
                    is-valid? (or (and (= "M" val-1) (= "S" val-2))
                                  (and (= "S" val-1) (= "M" val-2)))]
                (and acc is-valid?)))
            true allowed-translations)))

(defn part-2 [puzzle-input]
  (let [grid-map (prepare-input puzzle-input)
        a-locations (->> grid-map
                         utils/grid-map->grid-coll
                         (filter #(= "A" (:val %))))]
    (->> a-locations
         (filter (partial is-a-valid? grid-map))
         count)))

(part-2 input)