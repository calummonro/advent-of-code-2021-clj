(require '[clojure.string :as str])
(require '[clojure.set])

(def problem-data (-> "day-4-data.txt"
                      (slurp)
                      (str/split #"\n\n")))

(def sample-data (-> "day-4-sample-data.txt"
                     (slurp)
                     (str/split #"\n\n")))

(defn parse-turns [input]
  (as-> (first input) v
    (str/split v #",")
    (mapv #(Integer/parseInt %) v)))

(defn parse-board [board-raw]
  (mapv (fn [row-raw]
          (as-> row-raw v
            (str/split v #" ")
            (filter #(not= "" %) v)
            (mapv #(Integer/parseInt %) v)))
        (str/split board-raw #"\n")))

(defn parse-boards [input]
  (->> (rest input)
    (mapv parse-board)))

(defn transpose [matrix]
  (apply map vector matrix))

(defn line-complete? [line turns]
  (clojure.set/subset? (set line) (set turns)))

(defn board-wins? [board completed-turns]
  (let [lines (concat board (transpose board))]
    (->> lines
         (map #(line-complete? % completed-turns))
         (some #{true}))))

(defn play [board turns]
  (loop [turn (first turns)
         remaining-turns (rest turns)
         completed-turns []]
    (if (board-wins? board completed-turns)
      {:board board
       :completed-turns completed-turns}
      (recur (first remaining-turns)
             (rest remaining-turns)
             (conj completed-turns turn)))))

(defn find-last-winner [boards turns]
  (->> boards
       (map #(play % turns))
       (sort-by #(count (:completed-turns %)))
       (last)))

(defn calculate-score [board turns]
  (* (last turns)
     (apply +
            (clojure.set/difference
             (set (flatten board))
             (set turns)))))

(defn calc [input]
  (let [boards (parse-boards input)
        turns (parse-turns input)]
    (let [{:keys [board completed-turns]} (find-last-winner boards turns)]
      (calculate-score board completed-turns))))
