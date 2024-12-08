(ns advent-of-code-clojure.grid-utils)

(def all-directions
  {:up {:y -1 :x 0}
   :down {:y 1 :x 0}
   :left {:y 0 :x -1}
   :right {:y 0 :x 1}
   :up-left {:y -1 :x -1}
   :up-right {:y -1 :x  1}
   :down-left {:y 1 :x -1}
   :down-right {:y 1 :x 1}})

(defn get-coordinates [grid-item] (select-keys grid-item [:x :y]))

(defn- indexed-grid-coll
  "Takes an input and returns a collection of values in the form
     `{:x X :y Y :val val}`, where x and y are co-ordinates and val is the
     value of the input, optionally transformed by xform if provided."
  [input & {:keys [xform]}]
  (flatten (map-indexed (fn [y-index row]
                          (map-indexed (fn [x-index val]
                                         {:x x-index :y y-index :val (if xform (xform val) val)})
                                       row))
                        input)))

(defn indexed-grid-map
  "Takes an input and returns a collection hashmap of values in the form
     `{:Y-1 {:X-1 val}}` where y and x are look up co-ordinates, and val is the
     value of the input, optionally transformed by xform if provided."
  [input & {:keys [xform] :as opts}]
  (let [indexed-values (indexed-grid-coll input opts)]
    (reduce (fn [acc {:keys [y x] :as item-with-coords}]
              (assoc-in acc [y x] item-with-coords))
            {} indexed-values)))

(defn grid-map->grid-coll [grid-map]
  (->> grid-map vals (mapcat vals)))

(defn get-indexed-grid-map-val
  "Given an associative structure of coordinates, returns the value at position Y X, or nil if it is not present"
  [indexed-map y x]
  (get-in indexed-map [y x]))

(defn- apply-translation [grid-item-1 grid-item-2 & {:keys [operation] :or {operation +}}]
  (merge-with operation (get-coordinates grid-item-1) (get-coordinates grid-item-2)))

(defn apply-translation-get-value [grid-map grid-item translation & {:keys [operation] :as opts}]
  (let [{next-y :y next-x :x} (apply-translation grid-item translation opts)]
    (get-indexed-grid-map-val grid-map next-y next-x)))

(defn get-value-in-direction [grid-map direction grid-item]
  (let [translation (get all-directions direction)
        {next-y :y next-x :x} (apply-translation grid-item translation)]
    (get-indexed-grid-map-val grid-map next-y next-x)))

(defn walk-grid [grid-map translation starting-grid-item & {:keys [steps] :or {steps 1}}]
  (let [res (loop [cur-step 0 items (list starting-grid-item)]
              (if (= cur-step steps)
                items
                (let [next-item (get-value-in-direction grid-map translation (first items))]
                  (if (nil? next-item)
                    items
                    (recur (inc cur-step) (cons next-item items))))))]
    (reverse res)))

(defn find-grid-item [grid-map target-val]
  (reduce (fn [_acc {:keys [val] :as grid-item}]
            (when (= val target-val)
              (reduced grid-item)))
          nil (grid-map->grid-coll grid-map)))

(defn replace-grid-item [grid-map new-value {:keys [y x] :as _grid-item}]
  (assoc-in grid-map [y x :val] new-value))

(defn calc-translation
  "Calculates the x y vector translation from grid item 1 to grid item 2"
  [grid-item-1 grid-item-2]
  (merge-with - (get-coordinates grid-item-2) (get-coordinates grid-item-1)))