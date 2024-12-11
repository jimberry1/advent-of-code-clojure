(ns advent-of-code-clojure.2024.day-11
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 11))
(def input (utils/load-input-file 2024 11))

(defn prepare-input [input]
  (utils/format-input input :val-xform parse-long :input-xform first))

(defn blink [stone]
  (let [stone-str (str stone)
        len (count stone-str)
        pivot (quot len 2)]
    (cond (zero? stone) [1]
          (even? len) (map parse-long [(subs stone-str 0 pivot) (subs stone-str pivot)])
          :else [(* stone 2024)])))

(def stone->stone-count-after-n-blinks
  ;; internal memoizsation is important because of recursive calls
  (memoize (fn [remaining-blinks stone]
             (if (zero? remaining-blinks)
               1
               (transduce (map #(stone->stone-count-after-n-blinks (dec remaining-blinks) %)) + (blink stone))))))

(time (->> input prepare-input (map #(stone->stone-count-after-n-blinks 25 %)) (reduce +)))
(time (->> input prepare-input (map #(stone->stone-count-after-n-blinks 75 %)) (reduce +)))