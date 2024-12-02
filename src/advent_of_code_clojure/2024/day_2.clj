(ns advent-of-code-clojure.2024.day-2
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 2))
(def input (utils/load-input-file 2024 2))

(defn row->tuple [row]
  (->> row
       (re-seq #"\d+")
       (map (fn [val] (Integer. val)))))

(defn is-gap-valid? [row]
  (let [res (reduce (fn [last next]
                      (let [dif (Math/abs (- next last))]
                        (if (or (> dif 3) (< dif 1))
                          (reduced :invalid)
                          next)))
                    row)]
    (not= res :invalid)))

(defn is-ordered? [row]
  (let [ordered-row (sort row)]
    (or (= row ordered-row)
        (= row (reverse ordered-row)))))

(defn is-row-valid? [row]
  (and (is-ordered? row) (is-gap-valid? row)))

(->> input
     (map row->tuple)
     (filter is-row-valid?)
     count)

;; part 2 
(defn drop-nth [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(defn is-valid-with-dampening? [row]
  (->> row
       count
       range
       (map #(drop-nth % row))
       (map is-row-valid?)
       (some #(true? %))))

(->> input
     (map row->tuple)
     (filter is-valid-with-dampening?)
     count)