(ns aoc22.src.day25
  (:require
    [aoc-tools :refer [read-input submit-answer]]))

(def input (read-input :test))
(def real-input (read-input))

(def dec-table {\2 2 \1 1 \0 0 \- -1 \= -2})
(def snafu-table (zipmap (vals dec-table) (keys dec-table)))
(def invert-table {\2 \= \1 \- \0 \0 \- \1 \= \2})

(defn to-decimal
  [snafu]
  (let [powers (take (count snafu) (iterate (partial * 5) 1))]
    (->> snafu
         reverse
         (map dec-table)
         (map * powers)
         (apply +))))

(defn invert
  [snafu]
  (apply str (map invert-table snafu)))

(defn exp
  [x n]
  (reduce * (repeat n x)))

(defn snafu
  [number]
  (if (<= -2 number 2)
    (str (get snafu-table number))
    (let [max-number-per-digit-count (->> (iterate #(str % "2") "2")
                                          (map to-decimal)
                                          (take-while #(< % number)))
          digits (inc (count max-number-per-digit-count))
          biggest-power (exp 5 (dec digits))
          max-for-1xxx (+ (last max-number-per-digit-count) biggest-power)
          first-digit (inc (quot number max-for-1xxx))
          first-char (get snafu-table first-digit)
          remainder (- number (* first-digit biggest-power))
          last-chars (if (pos? remainder)
                       (snafu remainder)
                       (invert (snafu (- remainder))))]
      (str first-char (apply str (repeat (- digits 1 (count last-chars)) \0)) last-chars))))

(defn console-number
  [input]
  (->> input
       (map to-decimal)
       (apply +)
       (snafu)))

(->> real-input
     console-number
     time
     #_(submit-answer 1))
