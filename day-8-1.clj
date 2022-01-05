(ns day-8-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-8-sample-data.txt"))
(def problem-data (slurp "day-8-problem-data.txt"))

(defonce magic-counts #{2 3 4 7})

(defn parse [data]
  (as-> data v
    (str/split v #"\n")
    (map #(take-last 4 (str/split %  #" ")) v)
    (flatten v)))

(defn find-uniques [patterns]
  (filter #(some (set [(count %)]) magic-counts) patterns))

(defn uniques-count [patterns]
  (count (find-uniques patterns)))
