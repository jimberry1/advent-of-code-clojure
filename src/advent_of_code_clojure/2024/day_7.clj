(ns advent-of-code-clojure.2024.day-7
  (:require [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(def example-input (utils/load-example-input 2024 7))
(def input (utils/load-input-file 2024 7))

(defn prepare-line [line]
  (let [target (-> line first (str/replace #":" "") utils/->long)
        combinators (->> line rest (map utils/->long))]
    {:target target :combinators combinators}))

(defn prepare-input [input]
  (utils/format-input input :line-xform prepare-line))

(defn combines-to-target? [allowed-operators {:keys [target combinators] :as _input-line}]
  (let [[base & rest-combinators] combinators
        totals (reduce (fn [totals next-number]
                         (->> totals
                              (mapcat (fn [total] (map #(% total next-number) allowed-operators)))
                              (remove #(> % target)))) [base] rest-combinators)]
    (some (partial = target) totals)))

(defn sum-allowed-totals [input allowed-operators]
  (let [parsed-input (prepare-input input)]
    (->> parsed-input
         (filter #(combines-to-target? allowed-operators %))
         (map :target)
         (reduce +))))

(def p1-operators (list * +))
(sum-allowed-totals input p1-operators)

;; part 2
(def p2-operators (list * + #(utils/->long (str %1 %2))))
(sum-allowed-totals input p2-operators)