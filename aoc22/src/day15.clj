(ns aoc22.src.day15
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers]]))

(def input (read-input :test))
(def real-input (read-input))

(defn distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn get-sensors
  [input]
  (->> input
       (map extract-numbers)
       (map #(partition 2 %))
       (map vec)
       (map #(vector (first %) (distance (first %) (second %))))
       (into {})))

(defn row-coverage-from-sensor
  [row [[x y] radius]]
  (let [row-distance (abs (- row y))
        col-distance (- radius row-distance)]
    (if (neg? col-distance)
      []
      [(- x col-distance) (+ x col-distance)])))

(defn row-coverage
  [row sensors]
  (->> sensors
       (reduce #(conj %1 (row-coverage-from-sensor row %2)) [])
       (remove empty?)
       (sort-by first)))

(defn find-hole-in-coverage
  [row max-range [r1 r2 & more]]
  (cond
    (pos? (first r1)) [0 row]
    (> (first r2) (inc (second r1))) [(inc (second r1)) row]
    (> (second r2) max-range) (if (= row 2000000)
                                (do (prn (- (second r2) (first r1))) nil ; Part 1
                                    nil))
    :else (find-hole-in-coverage row max-range (conj more [(first r1) (max (second r1) (second r2))]))))

(defn find-beacon
  [input max-range]
  (let [sensors    (get-sensors input)
        beacon-row (first (filter (fn [row] (->> sensors
                                                 (row-coverage row)
                                                 (find-hole-in-coverage row max-range)))
                                  (range max-range)))]
    (find-hole-in-coverage beacon-row max-range (row-coverage beacon-row sensors))))

(defn frequency
  [input max-range]
  (let [[x y] (find-beacon input max-range)]
    (+ (* 4000000 x) y)))

(->> (frequency real-input 4000000)
     time
     #_(submit-answer 2))




