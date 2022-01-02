(ns day-7-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-7-sample-data.txt"))
(def problem-data (slurp "day-7-problem-data.txt"))

(def positions (parse sample-data))

(defn parse [data]
  (as-> data v
      (str/split v #"\n")
      (first v)
      (str/split v #",")
      (mapv #(Integer/parseInt %) v)))

;; 1. loop from 0 - max(positions)
;; 2. for each position,
;;    - reduce to sum of distances from element to position

(defn get-total-for-number [positions number]
  (reduce (fn [total position]
            (let [delta (Math/abs (- position number))]
              (+ total delta)))
          0
          positions))

(defn find-smallest-total [positions]
  (let [max-position (apply max positions)
        position-range (range max-position)]
    (apply min (map (partial get-total-for-number positions)
                    position-range))))
