(ns day-1-2
  (:require [clojure.string :as str]))

(defn parse [data]
  (as-> data v
    (str/split-lines v)
    (mapv #(Integer/parseInt %) v)))

(defn calc [input]
  (reduce (fn [acc [a b]]
            (if (> (apply + b) (apply + a))
              (+ acc 1)
              acc))
          0
          (->> input
               (partition 3 1 input)
               (partition 2 1))))

(defn start []
  (-> "data/day-1-problem-data.txt"
      slurp
      parse
      calc))
