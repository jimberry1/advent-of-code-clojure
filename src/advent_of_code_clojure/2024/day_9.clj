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

(defn p2-expand [items]
  (reduce (fn [[file-map space-map current-pos index] next-item]
            (if (= (rem index 2) 0)
              (let [updated-file-map (assoc file-map current-pos [(/ index 2) next-item])]
                [updated-file-map space-map (+ current-pos next-item) (inc index)])
              (let [updated-space-map (assoc space-map current-pos next-item)]
                [file-map updated-space-map (+ current-pos next-item) (inc index)])))
          [(sorted-map) (sorted-map) 0 0] items))

(defn find-space [space-map file-size file-pos]
  (reduce (fn [_acc [space-pos space-size :as space]]
            (cond
              (> space-pos file-pos) (reduced nil)
              (<= file-size space-size) (reduced space)
              :else nil)) nil space-map))

(defn update-file-map [file-map space-pos file-pos file]
  (-> file-map
      (assoc space-pos file)
      (dissoc file-pos)))

(defn update-space-map [space-map [space-pos space-size] file-pos file-size]
  (cond-> space-map
    :always (dissoc space-pos)
    :always (assoc file-pos file-size)
    (< file-size space-size) (assoc (+ space-pos file-size) (- space-size file-size))))

(defn loop-file [initial-file-map initial-space-map]
  (loop [[[file-pos [_file-id file-size :as next-file]] & rem] (reverse initial-file-map) space-map initial-space-map res initial-file-map]
    (if (nil? next-file)
      res
      (let [[space-pos :as space] (find-space space-map file-size file-pos)]
        (if (nil? space-pos)
          (recur rem space-map res)
          (let [updated-res (update-file-map res space-pos file-pos next-file)
                updated-space-map (update-space-map space-map space file-pos file-size)]
            (recur rem updated-space-map updated-res)))))))

(defn solve-better [input]
  (let [[initial-file-map initial-space-map] (->> input prepare-input p2-expand)
        condensed-file (loop-file initial-file-map initial-space-map)]
    (->> condensed-file
         (transduce (mapcat (fn [[idx [file-id size]]]
                              (map #(* % file-id) (range idx (+ idx size))))) +))))

(time (solve-better input))