(ns advent-of-code-clojure.2024.day-9
  (:require [advent-of-code-clojure.utils :as utils]))

(def example-input (utils/load-example-input 2024 9))
(def input (utils/load-input-file 2024 9))

(defn prepare-input [input]
  (utils/format-input input :line-re-split #"" :val-xform utils/->int :input-xform first))

(defn expand-list-p1 [input]
  (flatten (map-indexed (fn [idx file-size]
                          (if (= (rem idx 2) 0)
                            (take file-size (repeat (/ idx 2)))
                            (take file-size (repeat "."))))
                        input)))

(defn reorganise-files-p1 [expanded-list]
  (loop [[head & rest-list] expanded-list from (reverse expanded-list) res []]
    (cond
      (nil? head) res
      (= head ".") (let [[end-val & rest-from] (drop-while (partial = ".") from)]
                     (recur rest-list rest-from (conj res end-val)))
      :else (recur rest-list from (conj res head)))))

(defn solve [input]
  (->> input
       prepare-input
       expand-list-p1
       reorganise-files-p1
       (take-while (partial not= "."))
       (map-indexed (fn [idx item]
                      (* idx item)))
       (reduce +)))

(time (solve input))

(defn create-or-insert-nested-set [map key space-pos]
  (if (contains? map key)
    (update map key conj space-pos)
    (assoc map key (sorted-set space-pos))))

(defn analyse-file-structure [items]
  (reduce (fn [[file-map space-map current-pos index] next-item]
            (if (= (rem index 2) 0)
              (let [updated-file-map (assoc file-map current-pos [(/ index 2) next-item])]
                [updated-file-map space-map (+ current-pos next-item) (inc index)])
              (let [updated-space-map (create-or-insert-nested-set space-map next-item current-pos)]
                [file-map updated-space-map (+ current-pos next-item) (inc index)])))
          [(sorted-map) (sorted-map) 0 0] items))

(defn find-available-space [space-map file-size file-pos]
  (->> space-map
       (keep (fn [[space-size space-positions]]
               (when (and (<= file-size space-size) (< (first space-positions) file-pos)) [(first space-positions) space-size])))
       (sort-by first)
       first))

(defn update-file-map [file-map space-pos file-pos file]
  (-> file-map
      (assoc space-pos file)
      (dissoc file-pos)))

(defn update-space-map [space-map [space-pos space-size] file-pos file-size]
  (let [new-space-size (- space-size file-size)]
    (cond-> space-map
      :always (update space-size disj space-pos)
      :always (create-or-insert-nested-set file-size file-pos)
      (> new-space-size 0) (create-or-insert-nested-set new-space-size (+ space-pos file-size)))))

(defn loop-file [initial-file-map initial-space-map]
  (loop [[[file-pos [_file-id file-size :as next-file]] & rem] (reverse initial-file-map) space-map initial-space-map res initial-file-map]
    (if (nil? next-file)
      res
      (if-let [[space-pos :as space] (find-available-space space-map file-size file-pos)]
        (recur rem
               (update-space-map space-map space file-pos file-size)
               (update-file-map res space-pos file-pos next-file))
        (recur rem space-map res)))))

(defn solve-p2 [input]
  (let [[initial-file-map initial-space-map] (->> input prepare-input analyse-file-structure)
        condensed-file (loop-file initial-file-map initial-space-map)]
    (->> condensed-file
         (transduce (mapcat (fn [[idx [file-id size]]]
                              (map #(* % file-id) (range idx (+ idx size))))) +))))

(time (solve-p2 input))

;; The approach for part 2 is as follows:
;; Grouping all files into a map of their position -> id,size
;; Grouping all spaces into a map of their size -> position
;; Loop over files in reverse order, find all spaces large enough to fit
;; the fit, then take the space with the lowest position value
;; Update the file map to put moved block in space position and remove old file position
;; Update the space map to move or remove the occupied space, and add a space of file size at file location
;; Repeat until all files have been checked