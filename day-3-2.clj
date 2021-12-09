(require '[clojure.string :as str])

(def problem-data (-> "day-3-data.txt"
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
  (vec (for [entry input]
         (mapv (fn [char]
                 (Integer/parseInt char))
               (str/split entry #"")))))

(def matrix (to-matrix sample-data))

(defn transpose [matrix]
  (apply mapv vector matrix))

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

(defn vec->decimal [v]
  (Integer/parseInt (apply str v) 2))

(def find-o2-rating (find-rating bit-criteria-o2))
(def find-co2-rating (find-rating bit-criteria-co2))

(find-o2-rating matrix)
(find-co2-rating matrix)

(vec->decimal (find-o2-rating matrix))
(vec->decimal (find-co2-rating matrix))

(defn find-life-support-rating [data]
  (let [matrix (to-matrix data)
        o2-rating (find-o2-rating matrix)
        co2-rating (find-co2-rating matrix)]
    (* (vec->decimal o2-rating) (vec->decimal co2-rating))))

(= 230 (find-life-support-rating sample-data))

(find-life-support-rating problem-data)
