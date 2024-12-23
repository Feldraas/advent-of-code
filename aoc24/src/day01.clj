(ns aoc24.src.day01
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [parse-int nmap]]
   [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn split-coll
  "Separate the odd and even numbered items of a collection"
  [coll]
  [(take-nth 2 coll) (take-nth 2 (rest coll))])

(defn parse-lists
  "Extract the left and right columns of the input"
  [input]
  (->> input
       (map #(str/split % #"   "))
       (nmap 2 parse-int)
       (flatten)
       (split-coll)))

;; Part 1
(defn total-distance
  "Calculate total distance in part 1"
  [input]
  (->> input
       (parse-lists)
       (map sort)
       (#(interleave (first %) (second %)))
       (partition 2)
       (map (partial apply -))
       (map abs)
       (apply +)))

(->> real-input
     (total-distance)
     #_(submit-answer 1))

(defn similarity-score
  "Calculate similarity score for one item"
  [coll n]
  (->> coll
       (filter #{n})
       (count)
       (* n)))

(defn total-similarity-score
  "Calculate similarity score between two lists"
  [left right]
  (->> left
       (map (partial similarity-score right))
       (apply +)))

;; Part 2
(->> real-input
     (parse-lists)
     (apply total-similarity-score)
     #_(submit-answer 2))
