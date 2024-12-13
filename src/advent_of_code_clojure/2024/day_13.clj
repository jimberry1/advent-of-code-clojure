(ns advent-of-code-clojure.2024.day-13
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 13))
(def input (utils/load-input-file 2024 13))

(defn prepare-input [input]
  (->> input
       (map #(re-seq #"\d+" %))
       (remove nil?)
       (map #(map parse-long %))
       (partition 3)
       (map (fn [[[Ax Ay] [Bx By] [target-x target-y]]] [Ax Bx Ay By target-x target-y]))))

;; Equation
;; N Ax + M Bx = Px
;; N Ay + M By = Py

;; rearrange equation 1
;; M = (px - N Ax) / Bx

;; Substitute into equation equation 2
;; Py = N Ay + By * (Px - N Ax) / Bx
;; Py = N Ay + (Px By) / Bx - (N Ax By) / Bx
;; Py - (Px By) / Bx = N Ay - (N Ax By) / Bx
;; Py - (Px By) / Bx = N (Ay -  (Ax By) / Bx)
;; N = (Py - (Px By) / Bx) / (Ay -  (Ax By) / Bx)

;; We also know this only works when we have whole button presses (fractional button presses are not possible)
;; And finally to calculate the cost, multiply A by 3 as it costs 3 coins whilst B costs one coin
(defn calculate-cost [[ax bx ay by target-x target-y]]
  (let [n (/ (- target-y  (* (/ target-x bx) by)) (- ay (* (/ ax bx) by)))
        m (/ (- target-x (* n ax)) bx)]
    (if (or (ratio? n) (ratio? m))
      0
      (+ (* 3 n) m))))

(defn offset-targets [offset [ax bx ay by target-x target-y]]
  [ax bx ay by (+ offset target-x) (+ offset target-y)])

(defn solve [puzzle-input & {:keys [offset]}]
  (->> puzzle-input
       prepare-input
       (map (if offset (partial offset-targets offset) identity))
       (map calculate-cost)
       (reduce +)
       long))

(solve input)
(solve input :offset 10000000000000)
