.PHONY: new-day
new-day:
	@read -p "Enter year: " year; \
	read -p "Enter day: " day; \
	mkdir -p src/advent_of_code_clojure/$$year; \
	touch resources/$$year/day-$$day-example.txt; \
	echo "(ns advent-of-code-clojure.$$year.day-$$day\n  (:require [advent-of-code-clojure.utils :as utils]))\n\n(def example-input (utils/load-example-input $$year $$day))\n(def input (utils/load-input-file $$year $$day))" > src/advent_of_code_clojure/$$year/day_$$day.clj