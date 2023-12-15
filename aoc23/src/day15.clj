(ns aoc23.src.day15
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn hash-char
  [starting-value ch]
  (-> starting-value
      (+ (int ch))
      (* 17)
      (mod 256)))

(defn hash-string
  [s]
  (reduce hash-char 0 s))

(defn hash-sum
  [input]
  (->> (str/split (first input) #",")
       (map hash-string)
       (apply +)))

(->> (hash-sum real-input)
     time
     #_(submit-answer 1))