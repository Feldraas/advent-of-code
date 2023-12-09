(ns aoc21.src.day1
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [parse-int]]))

(def input (map parse-int (read-input :test)))
(def real-input (map parse-int (read-input)))

(defn number-of-increases
  [input]
  (->> (map - (rest input) input)
       (filter pos?)
       (count)))

;(submit-answer 1 (number-of-increases real-input)))

(defn number-of-three-window-increases
  [input]
  (->> (interleave input (rest input) (rest (rest input)))
       (partition 3)
       (map #(apply + %))
       (number-of-increases)))

;(submit-answer 2 (number-of-three-window-increases real-input))
