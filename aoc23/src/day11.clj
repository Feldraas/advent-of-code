(ns aoc23.src.day11
  (:require
    [aoc-tools :refer [read-input submit-answer]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-empty-rows
  [input]
  (->> (range (count input))
       (filter (fn [idx] (every? #{\.} (nth input idx))))))

(defn get-col
  [input col]
  (map #(nth % col) input))

(defn get-empty-cols
  [input]
  (->> (range (count (first input)))
       (filter (fn [idx] (every? #{\.} (get-col input idx))))))

(defn shift-galaxy
  [input times [x y]]
  (let [rows (get-empty-rows input)
        dx (->> rows
                (filter (partial > x))
                (count)
                (* (dec times)))
        cols (get-empty-cols input)
        dy (->> cols
                (filter (partial > y))
                (count)
                (* (dec times)))]
    [(+ x dx) (+ y dy)]))

(defn get-shifted-galaxies
  [input times]
  (let [galaxy-coords (for [x (range (count input))
                            y (range (count (first input)))
                            :when (= (nth (nth input x) y) \#)]
                        [x y])]
    (map (partial shift-galaxy input times) galaxy-coords)))

(defn distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn total-distance-sum
  [input times]
  (let [galaxy-list (get-shifted-galaxies input times)]
    (->> (for [g1 galaxy-list
               g2 galaxy-list
               :when (pos? (compare g1 g2))]
           (distance g1 g2))
         (apply +))))

(->> (total-distance-sum real-input 2)
     (time)
     #_(submit-answer 1))

(->> (total-distance-sum real-input 1000000)
     (time)
     #_(submit-answer 2))