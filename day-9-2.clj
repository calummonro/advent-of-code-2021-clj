(ns day-9-2
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (as-> line v
    (str/split v #"")
    (mapv #(Integer/parseInt %) v)))

(defn parse [data]
  (->> data
       (str/split-lines)
       (mapv parse-line)))

(defonce max-value 9)

(defonce dir-map {:top [0 -1]
                  :right [1 0]
                  :bottom [0 1]
                  :left [-1 0]})

(defn get-top-loc [loc]
  (mapv + loc (:top dir-map)))

(defn get-right-loc [loc]
  (mapv + loc (:right dir-map)))

(defn get-bottom-loc [loc]
  (mapv + loc (:bottom dir-map)))

(defn get-left-loc [loc]
  (mapv + loc (:left dir-map)))

(defn find-adjacents [loc]
  [(get-top-loc loc)
   (get-right-loc loc)
   (get-bottom-loc loc)
   (get-left-loc loc)])

(defn local-min? [data loc]
  (let [value (get-in data loc)]
    (->> loc
         find-adjacents
         (map #(get-in data % max-value))
         (filter #(< % value))
         (empty?))))

;; arbitrary choice: take the first one which is lower
(defn pick-next-loc [data loc]
  (let [value (get-in data loc)]
    (->> loc
         find-adjacents
         (filter #(< (get-in data % max-value) value))
         first)))

(defn find-basin [data loc]
  (loop [current-loc loc]
    (cond
      (= 9 (get-in data loc))
      nil

      :else
      (if (local-min? data current-loc)
        current-loc
        (recur (pick-next-loc data current-loc))))))

;; note x/y is sort of flipped due to how we look up 2d vectors (down, then across)
(defn get-locs [data]
  (for [x (range (count data))
        y (range (count (first data)))]
    [x y]))

(defn calc [data]
  (->> data
       get-locs
       (map #(find-basin data %))
       (remove nil?)
       (frequencies)
       (sort-by val)
       (take-last 3)
       (map second)
       (reduce *)))

(defn start []
  (-> "day-9-problem-data.txt"
      slurp
      parse
      calc))
