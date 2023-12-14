(ns aoc23.src.day14
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [transpose]]))

(def input (read-input :test))
(def real-input (read-input))

(defn roll-row
  [row direction]
  (let [char-to-move (if (pos? (apply + direction))
                       #{\O}
                       #{\.})]
    (->> (partition-by #{\#} row)
         (map #(sort-by char-to-move %))
         flatten
         (apply str))))

(defn roll-all
  ([state]
   (roll-all state [-1 0]))
  ([state direction]
   (if (zero? (first direction))
     (->> state
          (map #(roll-row % direction)))
     (-> state
         transpose
         (roll-all (reverse direction))
         transpose))))

(defn total-load
  [state]
  (let [xmax (count state)
        ymax (apply max (map count state))]
    (->> (for [x (range xmax)
               y (range ymax)
               :let [ch (nth (nth state x) y)]
               :when (= ch \O)]
           (- xmax x))
         (apply +))))

(->> real-input
     roll-all
     total-load
     time
     #_(submit-answer 1))

(defn n-cycle?
  [coll n]
  (->> coll
       (take-last (* 2 n))
       (partition n)
       (apply =)))

(defn roll-cycle
  [state]
  (reduce roll-all state [[-1 0] [0 -1] [1 0] [0 1]]))

(defn find-cycle
  ([state]
   (find-cycle state 200))
  ([state steps-to-check]
   (let [checked-states (take steps-to-check (iterate roll-cycle state))
         length (->> (range 1 (int (inc (/ (count checked-states) 2))))
                     (filter #(n-cycle? checked-states %))
                     first)]
     (if length
       [length (take-last length checked-states) steps-to-check]
       (find-cycle state (* 2 steps-to-check))))))

(defn load-after-n-cycles
  [n state]
  (let [[cycle-length rotation steps-checked] (find-cycle state)
        index (mod (- n steps-checked) cycle-length)
        final-state (nth rotation index)]
    (total-load final-state)))

(->> real-input
     (load-after-n-cycles 1000000000)
     time
     #_(submit-answer 2))
