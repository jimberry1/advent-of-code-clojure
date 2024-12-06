(ns advent-of-code-clojure.2024.day-5
  (:require [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(def example-input (utils/load-example-input 2024 5))
(def input (utils/load-input-file 2024 5))

(defn- section-1->page-rules
  "Reduces section-1 of the puzzle into a map of page numbers to pages that must NOT preceed them."
  [rules]
  (let [unformatted-rules (->> rules (map #(str/split % #"\|")) (map #(map utils/->int %)))]
    (reduce (fn [acc-map [before-page target-num]]
              (merge-with into acc-map {before-page #{target-num}}))
            {} unformatted-rules)))

(defn- prepare-input [input]
  (let [inputs (split-at (.indexOf input "") input)
        page-rules (->> inputs first section-1->page-rules)
        current-orders (->> inputs second (drop 1) (map #(str/split % #",")) (map #(map utils/->int %)))]
    {:page-rules page-rules :orders current-orders}))

(defn- is-page-at-index-valid? [page-rules page-index order]
  (let [page-number (nth order page-index)]
    (nil? (when-let [forbidden-pages (get page-rules page-number)]
            (->> order
                 (take page-index)
                 (some forbidden-pages))))))

(defn- is-valid-order? [before-page-num-lookup order]
  (->> order
       count
       range
       (map #(is-page-at-index-valid? before-page-num-lookup % order))
       (every? true?)))

(defn- find-middle-val [order]
  (nth order (Math/floor (/ (count order) 2))))

(defn- part-1 [input]
  (let [{:keys [page-rules orders]} (prepare-input input)]
    (->> orders
         (filter #(is-valid-order? page-rules %))
         (map find-middle-val)
         (reduce +))))

(part-1 input)

(defn find-earliest-index-of-error [order targets]
  (loop [remaining order index 0]
    (let [next-item (first remaining)]
      (cond
        (nil? next-item) nil
        (targets next-item) index
        :else (recur (rest remaining) (inc index))))))

(defn adjust-order [page-rules erroneous-index order]
  (let [earliest-invalid-index (->> erroneous-index (nth order) (get page-rules) (find-earliest-index-of-error order))]
    (utils/move-item erroneous-index earliest-invalid-index order)))

(defn correct-page-order [page-rules order]
  (loop [current-order order index 0]
    (cond
      (= index (count order)) current-order
      (is-page-at-index-valid? page-rules index current-order) (recur current-order (inc index))
      :else (recur (adjust-order page-rules index current-order) 0))))

(defn part-2 [input]
  (let [{:keys [page-rules orders]} (prepare-input input)]
    (->> orders
         (remove #(is-valid-order? page-rules %))
         (map #(correct-page-order page-rules %))
         (map find-middle-val)
         (reduce +))))

(part-2 input)