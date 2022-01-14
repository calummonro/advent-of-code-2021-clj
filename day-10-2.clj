(ns day-10-2
  (:require [clojure.string :as str]))

(defonce left-paren \()
(defonce right-paren \))
(defonce left-square \[)
(defonce right-square \])
(defonce left-curly \{)
(defonce right-curly \})
(defonce left-angle \<)
(defonce right-angle \>)

(defonce match-map {\( \)
                    \[ \]
                    \{ \}
                    \< \>})

(defonce score-map {\( 1
                    \[ 2
                    \{ 3
                    \< 4})

(defonce opening-chars #{left-paren left-square left-curly left-angle})
(defonce closing-chars #{right-paren right-square right-curly right-angle})

(defn opening-char? [char]
  (boolean (opening-chars char)))

(defn closing-char? [char]
  (boolean (closing-chars char)))

(defn find-incomplete-chars [line]
  (loop [remaining line
         openings []]
    (let [char (first remaining)]
      (if (nil? char)
        openings
        (cond

          (opening-char? char)
          (recur (rest remaining)
                 (conj openings char))

          (closing-char? char)
          (if (= char (match-map (last openings)))
            (recur (rest remaining)
                   (pop openings))
            nil)

          :else (throw (Exception. "invalid char")))))))

(defn get-middle-element [v]
  (as-> v w
    (count w)
    (quot w 2)
    (nth v w)))

(defn calculate-score [incomplete-chars]
  (->> incomplete-chars
       (map #(score-map %))
       (reduce #(+ %2 (* 5 %1)) 0)))

(defn find-puzzle-answer [lines]
  (as-> lines v
    (map find-incomplete-chars v)
    (remove empty? v)
    (map reverse v)
    (map calculate-score v)
    (sort v)
    (get-middle-element v)))

(defn start []
  (-> "day-10-problem-data.txt"
      slurp
      str/split-lines
      find-puzzle-answer))
