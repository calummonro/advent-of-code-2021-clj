(ns day-8-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-8-sample-data.txt"))
(def problem-data (slurp "day-8-problem-data.txt"))

(defn parse [data]
  (as-> data v
    (str/split v #"\n")
    (map #(take-last 4 (str/split %  #" ")) v)
    (flatten v)))

(def numbers (parse sample-data))

(defn find-uniques [numbers]
  (filter (fn [number]
            (let [segment-count (count number)]
              (some (set [segment-count]) [2 3 4 7])))
          numbers))

(defn calc [numbers]
  (count (find-uniques numbers)))
