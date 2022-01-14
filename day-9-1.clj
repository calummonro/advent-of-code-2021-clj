(ns day-9-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-9-sample-data.txt"))
(def problem-data (slurp "day-9-problem-data.txt"))

(defn parse-line [line]
  (as-> line v
    (str/split v #"")
    (mapv #(Integer/parseInt %) v)))

(defn parse [data]
  (->> data
    (str/split-lines)
    (mapv parse-line)))

(defn find-adjacents [data [i j]]
  (let [top (get-in data [(- i 1) j])
        right (get-in data [i (+ j 1)])
        bottom (get-in data [(+ i 1) j])
        left (get-in data [i (- j 1)])]
    {:value (get-in data [i j])
     :adjacents (vec (remove nil? [top right bottom left]))}))

(defn local-min? [value adjacents]
  (< value (apply min adjacents)))

(defn find-local-mins [data]
  (for [[row i] (map vector data (range (count data)))
        j       (range (count row))
        :let [{:keys [value adjacents]} (find-adjacents data [i j])]
        :when (local-min? value adjacents)]
    value))

(defn find-total-risk-level [data]
  (->> data
       find-local-mins
       (map inc)
       (reduce +)))
