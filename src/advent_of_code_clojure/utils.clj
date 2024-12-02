(ns advent-of-code-clojure.utils
  (:require [clj-http.client :as http]
            [advent-of-code-clojure.config :as config]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- file-exists? [file-path]
  (let [file (io/file file-path)]
    (.exists file)))

(defn- ->input-filename [year day]
  (str "resources/" year "/day-" day "-input.txt"))

(defn- ->example-filename [year day]
  (str "resources/" year "/day-" day "-example.txt"))

(defn- fetch-input [year day]
  (let [endpoint (str "https://adventofcode.com/" year "/day/" day "/input")
        {:keys [body] :as response} (http/get endpoint {:headers {"Cookie" (str "session=" config/aoc-session)}})]
    body))

(defn- create-input-file [year day]
  (let [filename (->input-filename year day)
        problem-input (fetch-input year day)]
    (io/make-parents filename)
    (spit filename problem-input)))

(defn- read-input-file [filename]
  (-> filename
      slurp
      str/split-lines))

(defn load-input-file [year day]
  (let [filename (->input-filename year day)]
    (if (file-exists? filename)
      (read-input-file filename)
      (do
        (create-input-file year day)
        (read-input-file filename)))))

(defn load-example-input [year day]
  (let [example-filename (->example-filename year day)]
    (read-input-file example-filename)))

;; input transformation
(defn format-input
  "Transforms a puzzle input into a format preferable for solving.
   
   Optional parameters
   :line-re-split - a regex pattern to split puzzle lines into a collection of values
   :input-xform   - a fn of form (input) -> val that transforms the puzzle input
   :line-xform    - a fn of form (row) -> val that transforms the puzzle line
   :val-xform     - a fn of form (val) -> val that transforms a value"
  [input & {:keys [line-re-split input-xform line-xform val-xform]
            :or {line-re-split #"\s+" input-xform identity line-xform identity val-xform identity}}]
  (let [rows (->> input
                  (map #(str/split % line-re-split))
                  (map #(map val-xform %))
                  (map line-xform))]
    (input-xform rows)))

(defn transpose
  "Tranposes a matrix of values, e.g.
   [[1 2 3] [4 5 6]] -> [[1 4] [2 5] [3 6]]"
  [m]
  (apply mapv vector m))

(defn indexed-grid-coll
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
    (reduce (fn [acc {:keys [y x val]}]
              (assoc-in acc [y x] val))
            {} indexed-values)))

(defn get-indexed-grid-map-val
  "Given an associative structure of coordinates, returns the value at position Y X, or nil if it is not present"
  [indexed-map y x]
  (get-in indexed-map [y x]))

(defn ->int [val]
  (Integer. val))
