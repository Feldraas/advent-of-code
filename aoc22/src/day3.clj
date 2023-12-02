(ns aoc22.src.day3
  (:require [utils :refer [read-input]]
            [clojure.set :refer [intersection]]))

(def contents (read-input "day3.txt"))

(defn split-compartments
  [string]
  (let [half (/ (count string) 2)]
    [(take half string) (take-last half string)]))

(defn find-repeated-item
  [comps]
  (->> comps
       (map set)
       (apply intersection)
       (first)))

(defn calc-priority
  [item]
  (if (< (int item) 97)
    (- (int item) 38)
    (- (int item) 96)))

(->> contents
     (map split-compartments)
     (map find-repeated-item)
     (map calc-priority)
     (apply +))

(->> contents
     (partition 3)
     (map find-repeated-item)
     (map calc-priority)
     (apply +))