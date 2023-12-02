(ns aoc22.src.day4
  (:require [utils :refer [read-input
                           parse-int]]
            [clojure.set :refer [intersection
                                 subset?]]
            [clojure.string :as str]))

(def pairs (read-input "day4.txt"))

(defn str->set
  [string]
  (let [[start end] (str/split string #"-")]
    (set (range (parse-int start) (+ (parse-int end) 1)))))


(->> pairs
     (map #(str/split % #","))
     (map #(map str->set %))
     (map #(or (subset? (first %) (last %))
               (subset? (last %) (first %))))
     (filter true?)
     (count))

(->> pairs
     (map #(str/split % #","))
     (map #(map str->set %))
     (map #(intersection (first %) (last %)))
     (filter not-empty)
     (count))