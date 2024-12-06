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

(defn ->int [val]
  (Integer. val))

(defn drop-nth [n coll]
  (keep-indexed #(when (not= %1 n) %2) coll))

(defn insert-at-index [n item coll]
  (let [[start end] (split-at n coll)]
    (concat start [item] end)))

(defn move-item [original-index desired-index collection]
  (let [item (nth collection original-index)]
    (->> collection
         (drop-nth original-index)
         (insert-at-index desired-index item))))