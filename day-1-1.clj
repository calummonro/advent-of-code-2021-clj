(ns day-1-1
  (:require [clojure.string :as str]))

(defn parse [data]
  (as-> data v
    (str/split-lines v)
    (mapv #(Integer/parseInt %) v)))

(defn calc [input]
  (->> input
       (partition 2 1)
       (reduce (fn [acc [a b]]
                 (if (> b a) (+ acc 1) acc))
               0)))

(defn start []
  (-> "data/day-1-problem-data.txt"
      slurp
      parse
      calc))
