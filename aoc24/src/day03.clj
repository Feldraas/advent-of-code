(ns aoc24.src.day03
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn filter-instructions
  [s]
  (->> s
       (str "do()")
       (#(str/replace % #"do\(\)" "\ndo()"))
       (#(str/replace % #"don't\(\)" "\ndon't()"))
       (#(str/split % #"\n"))
       (filter #(str/starts-with? % "do()"))
       (apply str)))

;; Part 1 and 2
(->> real-input
     (apply str)
     (filter-instructions) ;; Comment out for part 1
     (re-seq #"mul\(\d+,\d+\)")
     (map #(re-seq #"\d+" %))
     (map #(*
            (Integer/parseInt (first %))
            (Integer/parseInt (second %))))
     (apply +)
     #_(submit-answer 2))


