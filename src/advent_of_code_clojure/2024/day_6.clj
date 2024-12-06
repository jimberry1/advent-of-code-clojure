(ns advent-of-code-clojure.2024.day-6
  (:require [advent-of-code-clojure.utils :as utils]
            [advent-of-code-clojure.grid-utils :as grid]))

(def example-input (utils/load-example-input 2024 6))
(def input (utils/load-input-file 2024 6))

(defn prepare-input [input]
  (utils/format-input input :line-re-split #"" :input-xform grid/indexed-grid-map))

(def directions-update {:up :right :right :down :down :left :left :up})

(defn walk-grid [grid-map start-item]
  (loop [steps (list start-item) cur-item start-item direction :up]
    (let [next-item (grid/get-value-in-direction grid-map direction cur-item)]
      (cond
        (nil? next-item) steps
        (= (:val next-item) "#") (recur steps cur-item (directions-update direction))
        :else (recur (cons next-item steps) next-item direction)))))

(defn part-1 [input]
  (let [grid-map (prepare-input input)
        start-item (grid/find-grid-item grid-map "^")
        steps (walk-grid grid-map start-item)]
    (->> steps distinct count)))

(part-1 input)

;; part 2 - Aint no thang like brute force
(defn is-infinite-loop? [grid-map start-item]
  (loop [steps #{{:direction :up :item start-item}} cur-item start-item direction :up]
    (let [next-item (grid/get-value-in-direction grid-map direction cur-item)
          step {:direction direction :item next-item}]
      (cond
        (nil? next-item) false
        (steps step) true
        (= (:val next-item) "#") (recur steps cur-item (directions-update direction))
        :else (recur (conj steps step) next-item direction)))))

(defn part-2 [input]
  (let [grid-map (prepare-input input)
        start-item (grid/find-grid-item grid-map "^")
        steps (walk-grid grid-map start-item)]
    (->> steps
         distinct
         (remove (partial = start-item))
         (map (partial grid/replace-grid-item grid-map "#"))
         (filter #(is-infinite-loop? % start-item))
         count)))

(part-2 input)