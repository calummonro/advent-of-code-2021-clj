(ns day-1-2
  (:require [clojure.string :as str]))

(defn parse [data]
  (as-> data v
    (str/split-lines v)
    (mapv #(Integer/parseInt %) v)))

(defn calc [measurements]
  (->> measurements
       (partition 3 1)
       (partition 2 1)
       (filter (fn [[a b]] (> (reduce + b) (reduce + a))))
       count))

(defn start []
  (-> "data/day-1-problem-data.txt"
      slurp
      parse
      calc))
