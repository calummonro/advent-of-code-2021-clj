(ns day-3-2
  [:require [clojure.string :as str]])

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

(defn vec->decimal [v]
  (Integer/parseInt (apply str v) 2))

;; Criteria logic - use frequencies fn to count bits,
;; then compare counts depending on criteria
(defn create-bit-criteria [criteria-fn tie-breaker]
  (fn [the-map]
    (if (apply = (vals the-map))
      tie-breaker
      (key (apply criteria-fn val the-map)))))

(def bit-criteria-o2 (create-bit-criteria max-key 1))
(def bit-criteria-co2 (create-bit-criteria min-key 0))

(defn get-most-common-bit [matrix bit-position criteria-fn]
  (let [transposed (transpose matrix)
        column (get transposed bit-position)
        freqs (frequencies column)]
    (criteria-fn freqs)))

;; Main logic - iterate through bit-positions (via transposed matrix)
;; and pass the matrix through a reduction, whittling it down to
;; the last remaining number
(defn find-rating [criteria-fn]
  (fn [matrix]
    (first (reduce (fn [matrix bit-position]
                     (if (= 1 (count matrix))
                       matrix
                       (let [most-common-bit (get-most-common-bit matrix bit-position criteria-fn)]
                         (vec (filter (fn [row]
                                        (when (= most-common-bit (get row bit-position))
                                          row)) matrix)))))
                   matrix
                   (range (count (transpose matrix)))))))



(def find-o2-rating (find-rating bit-criteria-o2))
(def find-co2-rating (find-rating bit-criteria-co2))

(defn calc [matrix]
  (let [o2-rating (find-o2-rating matrix)
        co2-rating (find-co2-rating matrix)]
    (* (vec->decimal o2-rating) (vec->decimal co2-rating))))

(defn start []
  (-> "data/day-3-problem-data.txt"
      slurp
      parse
      calc))
