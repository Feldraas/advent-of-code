(ns aoc23.src.day11
  (:require
    [aoc-tools :refer [read-input submit-answer]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-empty-rows
  [input]
  (->> input
       (map-indexed (fn [row-idx row] (if (every? #{\.} row) row-idx nil)))
       (remove nil?)))

(defn get-col
  [input col]
  (map #(nth % col) input))

(defn get-empty-cols
  [input]
  (->> (range (count (first input)))
       (map (fn [col-idx] (if (every? #{\.} (get-col input col-idx)) col-idx nil)))
       (remove nil?)))

(defn move-galaxy
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

(defn get-galaxies
  [input times]
  (->> (for [x (range (count input))
             y (range (count (first input)))
             :when (= (nth (nth input x) y) \#)]
         [x y])
       (map (partial move-galaxy input times))))

(defn distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn distance-sum
  [input times]
  (->> (for [g1 (get-galaxies input times)
             g2 (get-galaxies input times)
             :when (= (compare g1 g2) -1)]
         (distance g1 g2))
       (apply +)))


(->> (distance-sum real-input 2)
     (time)
     #_(submit-answer 1))

(->> (distance-sum real-input 1000000)
     (time)
     #_(submit-answer 2))