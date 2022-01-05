(ns day-6-2
  (:require [clojure.string :as str]))

(defonce sample-data (slurp "day-6-sample-data.txt"))
(defonce problem-data (slurp "day-6-problem-data.txt"))

(defn parse [data]
  (as-> data v
    (str/split v #"\n")
    (first v)
    (str/split v #",")
    (mapv #(Integer/parseInt %) v)))

(defn get-total-count [freq-map]
  (reduce + (vals freq-map)))

(defn get-next-freq-map [freq-map day]
  (assoc freq-map
         0 (get freq-map 1 0)
         1 (get freq-map 2 0)
         2 (get freq-map 3 0)
         3 (get freq-map 4 0)
         4 (get freq-map 5 0)
         5 (get freq-map 6 0)
         6 (+ (get freq-map 7 0) (get freq-map 0 0))
         7 (get freq-map 8 0)
         8 (get freq-map 0 0)))

(defn simulate [data day-count]
  (let [initial-fish-states (parse data)
        freq-map (frequencies initial-fish-states)]
    (->> (range day-count)
         (reduce get-next-freq-map freq-map)
         (get-total-count))))
