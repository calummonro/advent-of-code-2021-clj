(ns day-5-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-6-sample-data.txt"))
(def problem-data (slurp "day-6-problem-data.txt"))

(defn parse [data]
  (as-> data v
      (str/split v #"\n")
      (first v)
      (str/split v #",")
      (mapv #(Integer/parseInt %) v)))

(defn get-next-fish-state [fish-state]
  (if (= 0 fish-state)
    [6 8]
    (dec fish-state)))

(defn get-next-fish-states [fish-states]
  (vec (flatten (mapv get-next-fish-state fish-states))))

(defn simulate [data day-count]
  (let [initial-fish-states (parse data)]
    (loop [fish-states initial-fish-states
           day 0]
      (if (= day day-count)
        (count fish-states)
        (recur (get-next-fish-states fish-states)
               (inc day))))))

(= 5934 (simulate sample-data 80))

(simulate problem-data)
