(ns day-7-2
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-7-sample-data.txt"))
(def problem-data (slurp "day-7-problem-data.txt"))

(defn parse [data]
  (as-> data v
      (str/split v #"\n")
      (first v)
      (str/split v #",")
      (mapv #(Integer/parseInt %) v)))

(defn fuel-cost [x target-x]
  (let [steps (Math/abs (- x target-x))]
    (loop [step 1
           cost 0]
      (if (> step steps)
        cost
        (recur (inc step) (+ cost step))))))

(defn total-fuel-cost [positions target]
  (->> positions
       (map (partial fuel-cost target))
       (reduce +)))

(defn min-fuel-cost [positions]
  (let [max-position (apply max positions)
        targets (range max-position)]
    (->> targets
         (map (partial total-fuel-cost positions))
         (apply min))))
