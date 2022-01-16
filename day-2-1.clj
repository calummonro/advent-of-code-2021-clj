(ns day-2-1
  (:require [clojure.string :as str]))

;; line: "forward 5"
(defn parse-line [line]
  (as-> line v
    (str/split v #" ")
    ((fn [[dir amt]] [dir (Integer/parseInt amt)]) v)))

(defn parse [data]
  (->> data
    str/split-lines
    (mapv parse-line)))

(defn aggregate-instructions [instructions]
  (let [{:strs [forward up down]} (group-by first instructions)]
    [(apply + (map second forward))
     (- (apply + (map second down))
        (apply + (map second up)))]))

(defn calc [input]
  (->> input
      aggregate-instructions
      (reduce *)))

(defn start []
  (-> "data/day-2-problem-data.txt"
      slurp
      parse
      calc))
