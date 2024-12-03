(ns advent-of-code-clojure.2024.day-3
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 3))
(def input (utils/load-input-file 2024 3))

(->> input
     (mapcat #(re-seq #"mul\((\d+),(\d+)\)" %))
     (map (fn [match-nums] (->> match-nums rest (map utils/->int) (reduce *))))
     (reduce +))

(->> input
     (mapcat #(re-seq #"don't\(\)|do\(\)|mul\((\d+),(\d+)\)" %))
     (reduce (fn [{:keys [go?] :as acc} match]
               (let [match-term (first match)]
                 (cond
                   (= match-term "don't()") (assoc acc :go? false)
                   (= match-term "do()") (assoc acc :go? true)
                   (true? go?) (update acc :total + (->> match rest (map utils/->int) (reduce *)))
                   :else acc)))
             {:total 0 :go? true})
     :total)