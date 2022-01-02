(ns day-7-2
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

;; note after copmletion: the key for this was just to adjust this function,
;; instead of just incrementing by the differnece, we want to calculate the fuel used for a given difference
(defn calculate-fuel-used [delta]
  (loop [d 1
         fuel-used 0]
    (if (> d delta)
      fuel-used
      (recur (inc d) (+ fuel-used d)))))

(defn get-total-for-number [positions number]
  (println (str "number: " number))
  (reduce (fn [total position]
            (let [delta (Math/abs (- position number))
                  fuel-used (calculate-fuel-used delta)]
              (+ total fuel-used)))
          0
          positions))

(defn find-smallest-total [positions]
  (let [max-position (apply max positions)
        position-range (range max-position)]
    (println "poop")
    (apply min (map (partial get-total-for-number positions)
                    position-range))))
