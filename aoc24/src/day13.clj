(ns aoc24.src.day13
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [extract-numbers]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-systems
  [input]
  (->> input
       (remove #{""})
       (partition 3)
       (map (partial apply str))
       (map extract-numbers)))

(defn solve
  [[a c b d p1 p2 :as system]]
  (let [disc (- (* a d) (* b c))]
    (if-not (zero? disc)
      [(/ (- (* d p1) (* b p2)) disc) (/ (- (* a p2) (* c p1)) disc)])))

(defn move-prize
  [[a c b d p1 p2 :as system]]
  [a c b d (+ p1 10000000000000) (+ p2 10000000000000)])


(->> real-input
     (get-systems)
     (map move-prize)  ;; Disable for Part 1
     (map solve)
     (filter (partial every? int?))
     (map #(+ (* 3 (first %)) (second %)))
     (apply +)
     #_(submit-answer 2))
