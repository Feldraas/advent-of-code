(ns aoc21.src.day5
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers sgn]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-line-range
  [[x1 y1 x2 y2]]
  (let [x-diff (- x2 x1)
        y-diff (- y2 y1)
        dx     (sgn x-diff)
        dy     (sgn y-diff)]
    (map #(vector (+ x1 (* % dx)) (+ y1 (* % dy)))
         (range (inc (max (abs x-diff) (abs y-diff)))))))

(defn parse-line
  [state line part]
  (let [[x1 y1 x2 y2] (extract-numbers line)]
    (if (or (= x1 x2) (= y1 y2) (= part :part2))
      (reduce #(if (get %1 %2)
                 (update %1 %2 inc)
                 (assoc %1 %2 1))
              state
              (get-line-range [x1 y1 x2 y2]))
      state)))

(->> real-input
     (reduce #(parse-line %1 %2 :part1) {})
     vals
     (filter #(> % 1))
     count
     time
     #_(submit-answer 1))

(->> real-input
     (reduce #(parse-line %1 %2 :part2) {})
     vals
     (filter #(> % 1))
     count
     time
     #_(submit-answer 2))
