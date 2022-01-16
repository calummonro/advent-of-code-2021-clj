(ns day-3-1
  (:require [clojure.string :as str]))

(defn to-matrix [lines]
  (for [line lines]
    (as-> line v
      (str/split v #"")
      (mapv #(Integer/parseInt %) v))))

(defn parse [data]
  (->> data
       str/split-lines
       to-matrix))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn flip-bit [bit]
  (if (= \1 bit) "0" "1"))

(defn compute-bit [freqs]
  (->> freqs
       (reduce (fn [acc [binary-value count]]
                 (if (> count (get acc 1))
                   [binary-value count]
                   acc))
               [0 0]
               )
       first))

(defn find-gamma [data]
  (let [transposed (transpose data)]
    (->> transposed
         (map frequencies)
         (map compute-bit)
         (map str)
         (apply str))))

(defn find-epsilon [gamma]
  (->> gamma
       (map flip-bit)
       (apply str)))

(defn calc [input]
  (let [gamma (find-gamma input)
        epsilon (find-epsilon gamma)
        gamma-decimal (Integer/parseInt gamma 2)
        epsilon-decimal (Integer/parseInt epsilon 2)]
    (* gamma-decimal epsilon-decimal)))

(defn start []
  (-> "data/day-3-problem-data.txt"
      slurp
      parse
      calc))
