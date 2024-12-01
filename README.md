# advent-of-code-clojure

A repository to track Clojure submissions for advent of code.

## Prerequisites

Ensure the environment variable `AOC_SESSION` is set as an environment variable. This can be obtained from authentication on the official [Advent of Code](https://adventofcode.com/) site.

## Usage

To create a new entry run

`make new-day`

This prompts the user for the year and the day. The puzzle input will be downloaded and stored to `resources/YEAR/day-DAY-input.txt`.

An empty file for the problem example will be created under `resources/YEAR/day-DAY-example.txt` which will need to be copied over by the user.
