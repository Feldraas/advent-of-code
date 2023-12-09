(ns aoc23.src.day9
  (:require
    [utils :refer [read-input extract-numbers]]))

(def input (read-input :test))
(def real-input (read-input))

(defn predict
  [seq]
  (if (every? zero? seq)
    [0 0]
    (let [[previous next] (predict (map - (rest seq) seq))]
      [(- (first seq) previous) (+ (last seq) next)])))

(time (->> real-input                                       ; Part 1+2
           (map extract-numbers)
           (map predict)
           (reduce (partial map +))))