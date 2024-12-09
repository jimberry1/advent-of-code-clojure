(ns advent-of-code-clojure.2024.day-9
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 9))
(def input (utils/load-input-file 2024 9))

(defn prepare-input [input]
  (utils/format-input input :line-re-split #"" :val-xform utils/->int :input-xform first))

(defn expand-list [input]
  (flatten (map-indexed (fn [idx file-size]
                          (if (= (rem idx 2) 0)
                            (take file-size (repeat (/ idx 2)))
                            (take file-size (repeat "."))))
                        input)))

(defn condense-list [expanded-list]
  (loop [[head & rest-list] expanded-list from (reverse expanded-list) res []]
    (cond
      (nil? head) res
      (= head ".") (let [[end-val & rest-from] (drop-while (partial = ".") from)]
                     (recur rest-list rest-from (conj res end-val)))
      :else (recur rest-list from (conj res head)))))

(defn solve [input]
  (->> input
       prepare-input
       expand-list
       condense-list
       (take-while (partial not= "."))
       (map-indexed (fn [idx item]
                      (* idx item)))
       (reduce +)))

(time (solve input))

(defn new-expand-list [idx item]
  (if (= (rem idx 2) 0)
    {:index (/ idx 2) :size item}
    {:index nil :size item}))

(defn update-space [space-index {:keys [size] :as replacement-item} vector]
  (let [updated-space (-> vector (get space-index) (update :size - size))
        item-idx (.indexOf vector replacement-item)
        updated-vector (assoc-in vector [item-idx :index] nil)]
    (vec (concat (subvec updated-vector 0 space-index)
                 [replacement-item updated-space]
                 (subvec updated-vector (inc space-index))))))

(defn find-gap-and-replace [order {target-index :index required-size :size :as item}]
  (loop [[{:keys [index size]} & rem-order] order idx 0]
    (cond
      (and (nil? index) (<= required-size size)) (update-space idx item order)
      (= target-index index) order
      :else (recur rem-order (inc idx)))))

(defn reduce-p2 [vals]
  (reduce (fn [updated-order {:keys [index] :as item}]
            (if (nil? index)
              updated-order
              (find-gap-and-replace updated-order item))) vals (reverse vals)))

(defn solve-p2 [input]
  (->> input
       prepare-input
       (map-indexed new-expand-list)
       (into [])
       reduce-p2
       (remove #(= 0 (:size %)))
       (mapcat (fn [{:keys [index size]}]
                 (take size (repeat index))))
       (map-indexed (fn [idx val] (when val (* val idx))))
       (remove nil?)
       (reduce +)))

(time (solve-p2 input))