(ns aoc22.src.day1
  (:require [utils :refer [read-input
                           parse-int]]))

(def calories (->> (read-input "day1.txt")
                   (map parse-int)
                   (partition-by zero?)
                   (map (partial apply +))
                   (sort)))

[(last calories), (apply + (take-last 3 calories))]
