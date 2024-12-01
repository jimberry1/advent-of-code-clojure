(ns advent-of-code-clojure.config
  (:require [environ.core :refer [env]]))

(def aoc-session (env :aoc-session))