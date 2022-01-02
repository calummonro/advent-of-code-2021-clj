(ns day-8-2
  (:require [clojure.string :as str]
            [clojure.set]))

(def sample-data (slurp "day-8-sample-data.txt"))
(def problem-data (slurp "day-8-problem-data.txt"))

(defn create-input-map [row]
  {:digits (vec (sort-by count (take 10 row)))
   :output (vec (take-last 4 row))})

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

;; definitions:
;; segments: :A, :B, :C (original)
;; segments :a, :b, :c (encoded)
;; numbers :1, :2, :3

;; need to create some rules
;; start with :1 - has two segments
;; also :7 has 3 segments, two shared with :1, so the extra segment is :A
;; next, find :D by comparing the 4 with the set of horizontal segments (foudn by intersecting the five counts)
;; now
;; next, find :G by taking the difference of the horizontals and 4
;; next, find :F by taking the intersection of 1 with the (intersection of six-counts)
;; next, find :C by taking using :F and :1

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
  (let [cde-set (get-exclusive-elements six-counts)]
    (first (clojure.set/intersection cde-set one))))

(defn find-f [one six-counts]
  (let [c (find-c one six-counts)]
    (first (clojure.set/difference one #{c}))))

(defn find-b [four c d f]
  (first (clojure.set/difference four #{c d f})))

(defn find-e [eight a b c d f g]
  (first (clojure.set/difference eight #{a b c d f g})))

;; creates map from segments to wires (e.g. wrong->correct)
(defn create-segment-map [digits]
  (let [one (first (filter #(= 2 (count %)) digits))
        four (first (filter #(= 4 (count %)) digits))
        seven (first (filter #(= 3 (count %)) digits))
        eight (first (filter #(= 7 (count %)) digits))
        five-counts (filter #(= 5 (count %)) digits) ;; (2 3 5)
        six-counts (filter #(= 6 (count %)) digits) ;; (0 6 9)
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

(def set-to-int-map
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

(defn lookup-number [s]
  (get set-to-int-map s))

;; next - convert from "eb" -> 1
(defn map-to-wires [segment-map segments]
  (set (map #(% segment-map) segments)))

(defn to-int [values]
  (Integer/parseInt (str/join values)))

(defn decode [{:keys [digits output]}]
  (let [segment-map (create-segment-map digits)]
    (->> output
         (map #(map-to-wires segment-map %))
         (map lookup-number)
         (to-int))))

(defn calc [input-maps]
  (reduce #(+ %1 (decode %2)) 0 input-maps))
