(ns advent-of-code-clojure.2024.day-1
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 1))
(def input (utils/load-input-file 2024 1))

(defn row->tuple [row]
  (->> row
       (re-seq #"\d+")
       (map (fn [val] (Integer. val)))))

(defn find-diff [list1 list2]
  (map #(Math/abs (- %1 %2)) list1 list2))

(defn sum-diff [rows]
  (let [extracted-numbers (map row->tuple rows)
        first-column (->> extracted-numbers (map first) sort)
        second-column (->> extracted-numbers (map second) sort)]
    (reduce + (find-diff first-column second-column))))

;; part 1
(sum-diff input)

;; part 2
(defn calc-freq [list]
  (reduce (fn [acc-map item]
            (merge-with + acc-map {item 1})) {} list))

(defn sum-weighted-vals-transduce [rows]
  (let [extracted-numbers (map row->tuple rows)
        freq (->> extracted-numbers (map second) calc-freq)]
    (transduce (comp
                (map first)
                (map #(* (or (get freq %) 0) %)))
               + extracted-numbers)))

(sum-weighted-vals-transduce input)
