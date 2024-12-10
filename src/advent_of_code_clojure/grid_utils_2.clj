(ns advent-of-code-clojure.grid-utils-2
  (:require
   [advent-of-code-clojure.utils :as utils]))

(def all-directions
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]
   :up-left [-1 -1]
   :up-right [1 -1]
   :down-left [-1 1]
   :down-right [1 1]})

(defn ->grid-map [input & {:keys [xform] :or {xform identity}}]
  (mapv #(into [] (map xform) %) input))

(defn grid-map->grid-coll [grid-map]
  (->> grid-map vals (mapcat vals)))

(defn get-item-at-coords
  "Given an associative structure of coordinates, returns the value at position Y X, or nil if it is not present"
  [grid-map [x y :as coords]]
  (let [row (get grid-map y)]
    (get row x)))

(defn apply-translation [translation coords]
  (mapv + coords translation))

(defn get-coord->coord-translation
  "Calculates the x y coordinates translation from coordinates 1 to coordinates 2"
  [coords-1 coords-2]
  (mapv - coords-2 coords-1))

(defn step-in-direction [direction [x y :as coords]]
  (let [translation (get all-directions direction)]
    (mapv + coords translation)))

(defn walk-grid-in-direction [grid-map direction start-x start-y & {:keys [steps] :or {steps 1}}]
  (let [grid-items (loop [cur-step 0 current-coords [start-x start-y] items (list)]
                     (if (= cur-step steps)
                       items
                       (let [next-item (get-item-at-coords grid-map current-coords)]
                         (if (nil? next-item)
                           items
                           (recur (inc cur-step) (step-in-direction direction current-coords) (cons next-item items))))))]
    (reverse grid-items)))

(defn find-grid-item
  "Iterates through a grid-map, returning the coordinates ([x y]) 
   of the first instance of the target value, or nil if not found."
  [grid-map target-val]
  (let [coords (reduce (fn [col-index row]
                         (let [row-index (.indexOf row target-val)]
                           (if (not= -1 row-index)
                             (reduced [row-index col-index])
                             (inc col-index))))
                       0 grid-map)
        found? (= 2 (count coords))]
    (when found?
      coords)))

(defn replace-grid-item
  "Replaces a grid-map value at the supplied coordinates."
  [grid-map new-value [x y :as coords]]
  (let [row (get grid-map y)
        updated-row (vec (utils/replace-at-index x new-value row))]
    (->> grid-map
         (utils/replace-at-index y updated-row)
         vec)))

(defn group-coords-by-item [grid-map]
  (let [vals (for [y (range (count grid-map))
                   x (range (count (first grid-map)))]
               {:x x :y y :val (get-item-at-coords grid-map [x y])})]
    (reduce (fn [acc {:keys [x y val]}]
              (if (contains? acc val)
                (update acc val conj [x y])
                (assoc acc val (list [x y]))))
            {} vals)))