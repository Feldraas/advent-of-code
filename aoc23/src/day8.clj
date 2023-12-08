(ns aoc23.src.day8
  (:require [utils :refer [read-input]]))

(def input (read-input :test))
(def real-input (read-input))


(defn create-network
  [input]
  (->> (drop 2 input)
       (map #(re-seq #"[0-9A-Z]+" %))
       (map #(vector (first %) [(second %) (last %)]))
       (into {})))

(defn create-state
  [input start]
  {:position start
   :steps    (seq (first input))
   :network  (create-network input)})

(defn next-position
  [state]
  (let [step (first (:steps state))
        [left right] (get (:network state) (:position state))]
    (if (= step \L)
      left
      right)))

(defn arrived?
  [state pattern]
  (str/ends-with? (:position state) pattern))

(defn move
  [state]
  (-> state
      (assoc :position (next-position state))
      (update :steps rotate)))

(defn termination
  [input start-point end-point]
  (let [iters (->> (create-state input start-point)
                   (iterate move)
                   (drop 1)
                   (take-while #(not (arrived? % end-point))))
        num-iters (inc (count iters))
        final-state (move (last iters))
        final-position (:position final-state)]
    (if (= start-point end-point)
      [num-iters num-iters]
      [num-iters (last (termination input final-position final-position))])))

(time (first (termination real-input "AAA" "Z")))           ; Part 1

(defn get-starts
  [input]
  (->> input
       (create-network)
       (keys)
       (filter #(str/ends-with? % "A"))))

(time (->> real-input                                       ; Part 2
           (get-starts)
           (map #(termination real-input % "Z"))
           (map first)
           (reduce lcm)))
