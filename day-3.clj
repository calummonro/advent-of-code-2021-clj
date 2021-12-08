(require '[clojure.string :as str])

(def problem-data (-> "day-3-1-data.txt"
                      (slurp)
                      (str/split #"\n")))

(def sample-data ["00100"
                  "11110"
                  "10110"
                  "10111"
                  "10101"
                  "01111"
                  "00111"
                  "11100"
                  "10000"
                  "11001"
                  "00010"
                  "01010"])

(defn to-matrix [input]
  (for [entry input]
    (map (fn [char]
           (Integer/parseInt char))
         (str/split entry #""))))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn compute-bit [freqs]
  (first (reduce (fn [acc [binary-value count]]
                   (if (> count (get acc 1))
                     [binary-value count]
                     acc))
                 [0 0]
                 freqs)))

(defn find-gamma [data]
  (let [transposed (transpose (to-matrix data))]
    (->> transposed
         (map frequencies)
         (map compute-bit)
         (map str)
         (apply str))))

(find-gamma sample-data)

(defn flip-bit [bit]
  (if (= \1 bit) "0" "1"))

(defn find-epsilon [gamma]
  (apply
   str
   (map flip-bit gamma)))

(find-epsilon (find-gamma sample-data))

(defn calc [input]
  (let [gamma (find-gamma input)
        epsilon (find-epsilon gamma)
        gamma-decimal (Integer/parseInt gamma 2)
        epsilon-decimal (Integer/parseInt epsilon 2)]
    (* gamma-decimal epsilon-decimal)))

(calc problem-data)

(= 198 (calc sample-data))
