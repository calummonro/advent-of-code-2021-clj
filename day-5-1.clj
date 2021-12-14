(ns day-5-1
  (:require [clojure.string :as str]))

(def sample-data (slurp "day-5-sample-data.txt"))
(def problem-data (slurp "day-5-problem-data.txt"))

(defn parse-coords [coords]
  (let [x1 (get-in coords [0 0])
        y1 (get-in coords [0 1])
        x2 (get-in coords [1 0])
        y2 (get-in coords [1 1])]
    {:x1 x1 :y1 y1
     :x2 x2 :y2 y2}))

;; input: ("0,1 -> 2,4", ...)
(defn parse [data]
  (as-> data v
    (str/split v #"\n")
    (mapv #(str/split % #" -> ") v)
    (mapv (fn [coords]
            (mapv (fn [coord]
                    (as-> coord w
                      (str/split w #",")
                      (mapv #(Integer/parseInt %) w)))
                  coords))
          v)
    (filterv (fn [coords]
               (let [{:keys [x1 y1 x2 y2]} (parse-coords coords)]
                 (or (= x1 x2) (= y1 y2))))
             v)))

(defn spread-line [coords]
  (let [{:keys [x1 y1 x2 y2]} (parse-coords coords)
        point-count-x (+ 1 (Math/abs (- x2 x1)))
        point-count-y (+ 1 (Math/abs (- y2 y1)))]
    (cond
      (= x1 x2) (vec (for [y (range point-count-y)]
                       (if (> y2 y1)
                         [x1 (+ y1 y)]
                         [x1 (- y1 y)])))
      (= y1 y2) (vec (for [x (range point-count-x)]
                       (if (> x2 x1)
                         [(+ x1 x) y1]
                         [(- x1 x) y1])))
      :else nil)))

(defn spread-lines [lines]
  (mapv spread-line lines))

(defn flatten-all-points [lines]
  (apply concat (spread-lines lines)))

(defn group-and-count [lines]
  (->> lines
       (flatten-all-points)
       (group-by identity)
       (filter #(> (count (val %)) 1))
       (count)))

(defn calc [input]
  (let [coords (parse input)]
    (group-and-count coords)))

(= 5 (calc sample-data))

(calc problem-data)
