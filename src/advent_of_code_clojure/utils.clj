(ns advent-of-code-clojure.utils
  (:require [clj-http.client :as http]
            [advent-of-code-clojure.config :as config]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn file-exists? [file-path]
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