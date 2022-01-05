(ns day-8-2
  (:require [clojure.string :as str]
            [clojure.set]))

(def sample-data (slurp "day-8-sample-data.txt"))
(def problem-data (slurp "day-8-problem-data.txt"))

(defn create-input-map [row]
  {:patterns (vec (sort-by count (take 10 row)))
   :reading (vec (take-last 4 row))})

(defn to-keyword-set [string]
  (as-> string v
    (str/split v #"")
    (mapv keyword v)
    (set v)))

(defn parse-row [row-data]
  (as-> row-data v
    (str/split v #" ")
    (mapv to-keyword-set v)
    (create-input-map v)))

(defn parse [data]
  (as-> data v
    (str/split-lines v)
    (mapv parse-row v)))

(defonce set-to-int-map
  {#{:C :F} 1
   #{:A :C :F} 7
   #{:B :C :D :F} 4
   #{:A :B :C :D :E :F :G} 8
   #{:A :C :D :E :G} 2
   #{:A :C :D :F :G} 3
   #{:A :B :D :F :G} 5
   #{:A :B :D :E :F :G} 6
   #{:A :B :C :D :F :G} 9
   #{:A :B :C :E :F :G} 0})

;; RULES FOR ASSIGNING WIRES TO SEGMENTS
;;
;; definitions:
;; segments: :A, :B, :C (original) - the intended outputs
;; wires: :a, :b, :c (encoded) - the inputs
;; numbers 1, 2, 3
;; five-counts: group of patterns which contain 5 active segments
;; six-counts: group of patterns which contain 6 active segments
;;
;; useful segment sets:
;;
;; "horizontal-segments"
;;   the set of the three horizontal segments :A, :D and :G
;;   found by intersecting the five-counts
;;
;; "c-d-e" set
;;   the arbitrary set of segments :C, :D, and :E
;;   found by intersecting the six-counts
;;
;; find :A by taking the difference between patterns representing 7 and 1
;; find :D by taking th intersection of 4 and the "horizontal-segments" set
;; find :G by taking the difference of the horizontals and the two known horizontals (:A and :D)
;; find :F by taking the intersection of 1 with the "c-d-e" set
;; find :C by taking the intersection of 1 and :F
;; find :B by taking the difference between 4 and the set of #{:C :D :F}
;; find :E by taking the difference of 8 with all other known segments

;; not sure if there's a name for this...
(defn get-exclusive-elements [sets]
  (clojure.set/difference
   (apply clojure.set/union sets)
   (apply clojure.set/intersection sets)))

(defn get-horizontal-segments [five-counts]
  (apply clojure.set/intersection five-counts))

(defn find-a [one seven]
  (first (clojure.set/difference seven one)))

(defn find-d [four five-counts]
  (let [horizontal-segments (get-horizontal-segments five-counts)]
    (first (clojure.set/intersection four horizontal-segments))))

(defn find-g [a d five-counts]
  (let [horizontal-segments (get-horizontal-segments five-counts)]
    (first (clojure.set/difference horizontal-segments #{a d}))))

(defn find-c [one six-counts]
  (let [c-d-e-set (get-exclusive-elements six-counts)]
    (first (clojure.set/intersection c-d-e-set one))))

(defn find-f [one six-counts]
  (let [c (find-c one six-counts)]
    (first (clojure.set/difference one #{c}))))

(defn find-b [four c d f]
  (first (clojure.set/difference four #{c d f})))

(defn find-e [eight a b c d f g]
  (first (clojure.set/difference eight #{a b c d f g})))

(defn find-cipher [patterns]
  (let [one (first (filter #(= 2 (count %)) patterns))
        four (first (filter #(= 4 (count %)) patterns))
        seven (first (filter #(= 3 (count %)) patterns))
        eight (first (filter #(= 7 (count %)) patterns))
        five-counts (filter #(= 5 (count %)) patterns)
        six-counts (filter #(= 6 (count %)) patterns)
        a (find-a one seven)
        d (find-d four five-counts)
        g (find-g a d five-counts)
        f (find-f one six-counts)
        c (find-c one six-counts)
        b (find-b four c d f)
        e (find-e eight a b c d f g)]
    {a :A
     b :B
     c :C
     d :D
     e :E
     f :F
     g :G}))

(defn decode-pattern [cipher pattern]
  (set (map #(% cipher) pattern)))

(defn decode-reading [{:keys [patterns reading]}]
  (let [cipher (find-cipher patterns)]
    (->> reading
         (map #(decode-pattern cipher %))
         (map set-to-int-map)
         (str/join)
         (Integer/parseInt))))

(defn sum-readings [input-maps]
  (->> input-maps
       (map decode-reading)
       (reduce +)))
