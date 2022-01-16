(ns day-1-1
  (:require [clojure.string :as str]))

(defn parse [data]
  (as-> data v
    (str/split-lines v)
    (mapv #(Integer/parseInt %) v)))

(defn calc [measurements]
  (->> measurements
       (partition 2 1)
       (filter (fn [[a b]] (> b a)))
       count))

(defn start []
  (-> "data/day-1-problem-data.txt"
      slurp
      parse
      calc))
