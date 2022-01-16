(require '[clojure.string :as str])
(require '[clojure.set])

(def problem-data (-> "data/day-4-problem-data.txt"
                      (slurp)
                      (str/split #"\n\n")))

(def sample-data (-> "day-4-sample-data.txt"
                     (slurp)
                     (str/split #"\n\n")))

(defn parse-turns [input]
  (as-> (first input) v
    (str/split v #",")
    (map #(Integer/parseInt %) v)
    (vec v)))

(defn parse-board [board-raw]
  (mapv (fn [row-raw]
          (as-> row-raw v
            (str/split v #" ")
            (filter #(not= "" %) v)
            (map #(Integer/parseInt %) v)
            (vec v)))
        (str/split board-raw #"\n")))

(defn parse-boards [input]
  (mapv parse-board (rest input)))

(defn transpose [matrix]
  (apply map vector matrix))

(defn check-line [line turns]
  (clojure.set/subset? (set line) (set turns)))

(defn check-board [board completed-turns]
  (let [lines (concat board (transpose board))]
    (->> lines
         (map #(check-line % completed-turns))
         (some #{true}))))

(defn get-winner [boards completed-turns]
  (loop [board (first boards)
         remaining-boards (rest boards)]
    (let [board-index (.indexOf boards board)]
      (cond
        (= true (check-board board completed-turns)) board-index
        (empty? remaining-boards) false
        :else (recur (first remaining-boards)
                     (rest remaining-boards))))))

;; game loop
(defn play [boards turns]
  (loop [turn (first turns)
         remaining-turns (rest turns)
         completed-turns []]
    (let [winner (get-winner boards completed-turns)]
      (cond
        (int? winner) {:winner (get boards winner)
                       :completed-turns completed-turns}
        (empty? remaining-turns) "out of turns"
        :else (recur (first remaining-turns)
                     (rest remaining-turns)
                     (conj completed-turns turn))))))

(defn calculate-score [board completed-turns]
  (apply +
         (clojure.set/difference
          (set (flatten board))
          (set completed-turns))))

(defn get-final-score [boards turns]
  (let [{:keys [winner completed-turns]} (play boards turns)
        score (calculate-score winner completed-turns)]
    (* score (last completed-turns))))

(defn calc [input]
  (let [boards (parse-boards input)
        turns (parse-turns input)]
    (get-final-score boards turns)))

(= 4512 (calc sample-data))
