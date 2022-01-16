(ns day-2-2
  [:require [clojure.string :as str]])

;; line: "forward 5"
(defn parse-line [line]
  (as-> line v
    (str/split v #" ")
    ((fn [[dir amt]] [dir (Integer/parseInt amt)]) v)))

(defn parse [data]
  (->> data
    str/split-lines
    (mapv parse-line)))

(defn calc-move-forward [[horizontal depth aim] value]
  [(+ horizontal value)
   (+ depth (* aim value))
   aim])

(defn calc-move-down [[horizontal depth aim] value]
  [horizontal depth (+ aim value)])

(defn calc-move-up [[horizontal depth aim] value]
  [horizontal depth (- aim value)])

(defn aggregate-instructions [instructions]
  (loop [props [0 0 0] ;; horizontal depth aim
         instruction (first instructions)
         remaining (rest instructions)]
    (if-not instruction
      [(first props) (second props)] ;; return [horizontal depth]
      (let [command (first instruction)
            value (last instruction)
            new-props (case command
                        "forward" (calc-move-forward props value)
                        "down" (calc-move-down props value)
                        "up" (calc-move-up props value))]
        (recur new-props (first remaining) (rest remaining))))))

(defn calc [input]
  (->> input
      aggregate-instructions
      (reduce *)))

(defn start []
  (-> "data/day-2-problem-data.txt"
      slurp
      parse
      calc))
