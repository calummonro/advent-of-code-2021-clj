(ns day-10-1
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

(defonce score-map {\) 3
                    \] 57
                    \} 1197
                    \> 25137})

(defonce opening-chars #{left-paren left-square left-curly left-angle})
(defonce closing-chars #{right-paren right-square right-curly right-angle})

(defn opening-char? [char]
  (boolean (opening-chars char)))

(defn closing-char? [char]
  (boolean (closing-chars char)))

(defn find-corrupted-char [line]
  (loop [remaining line
         openings []]
    (let [char (first remaining)]
      (if (nil? char)
        nil
        (cond

          (opening-char? char)
          (recur (rest remaining)
                 (conj openings char))

          (closing-char? char)
          (if (= char (match-map (last openings)))
            (recur (rest remaining)
                   (pop openings))
            char)

          :else
          (throw (Exception. "invalid char")))))))

(defn calculate-score [lines]
  (->> lines
       (map find-corrupted-char)
       (remove nil?)
       (map score-map)
       (reduce +)))

(defn start []
  (-> "day-10-problem-data.txt"
      slurp
      str/split-lines
      calculate-score))
