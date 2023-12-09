(ns aoc22.src.day1
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]))

(def calories (->> (read-input)
                   (map parse-int)
                   (partition-by zero?)
                   (map (partial apply +))
                   (sort)))

[(last calories), (apply + (take-last 3 calories))]
