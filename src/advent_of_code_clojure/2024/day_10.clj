(ns advent-of-code-clojure.2024.day-10
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils-2 :as grid-2]))

(def example-input (utils/load-example-input 2024 10))
(def input (utils/load-input-file 2024 10))

(defn prepare-input [input]
  (utils/format-input input :line-re-split #""))

(def possible-translations (vals (select-keys grid-2/all-directions [:up :down :left :right])))

(defn next-available-steps [allowed-next-coords coord]
  (->> possible-translations
       (map #(grid-2/apply-translation % coord))
       (keep allowed-next-coords)))

(defn walk-path [trail-val->coords start-coord only-distinct-paths?]
  (loop [trail-val 0 current-coords [start-coord]]
    (if (= 9 trail-val)
      (count current-coords)
      (let [next-trail-val (inc trail-val)
            all-next-trail-val-coords (get trail-val->coords next-trail-val)
            next-steps (cond->> current-coords
                         :always (mapcat #(next-available-steps all-next-trail-val-coords %))
                         only-distinct-paths? distinct)]
        (if (empty? next-steps)
          0
          (recur next-trail-val next-steps))))))

(defn solve [puzzle-input & {:keys [only-distinct-paths?] :or {only-distinct-paths? true}}]
  (let [grid-map (-> puzzle-input prepare-input (grid-2/->grid-map :xform utils/->int))
        trail-pos->coords-set (update-vals (grid-2/group-coords-by-item grid-map) set)
        start-coords (get trail-pos->coords-set 0)]
    (->> start-coords
         (map #(walk-path trail-pos->coords-set % only-distinct-paths?))
         (reduce +))))

(solve input)
(solve input :only-distinct-paths? false)