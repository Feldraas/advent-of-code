(ns aoc22.src.day8
  (:require
    [aoc-tools :refer [read-input]]))

(def grid (read-input))

(def side (count (first grid)))

(defn get-row
  [[x _]]
  (->> (nth grid x)
       (map int)
       (map #(- % 48))))

(defn get-col
  [[_ y]]
  (->> (map #(nth % y) grid)
       (map int)
       (map #(- % 48))))

(defn get-height
  [[x y]]
  (nth (get-row [x y]) y))

(defn visible?
  [tree]
  (let [[x y] tree]
    (cond
      (zero? x) true
      (zero? y) true
      (= x (dec side)) true
      (= y (dec side)) true
      :else (let [row (get-row tree)
                  col (get-col tree)
                  height (get-height tree)]
              (cond
                (> height (apply max (take y row))) true
                (> height (apply max (drop (inc y) row))) true
                (> height (apply max (take x col))) true
                (> height (apply max (drop (inc x) col))) true
                :else false)))))


(->> (for [x (range side)
           y (range side)]
       (visible? [x y]))
     (filter true?)
     (count))

(defn viewing-distance
  [height trees]
  (cond
    (empty? trees) 0
    (>= (first trees) height) 1
    :else (inc (viewing-distance height (rest trees)))))

(defn scenic-score
  [tree]
  (let [[x y] tree
        row (get-row tree)
        col (get-col tree)
        height (get-height tree)
        left (reverse (take y row))
        right (drop (inc y) row)
        up (reverse (take x col))
        down (drop (inc x) col)]
    (->> [left right up down]
         (map #(viewing-distance height %))
         (apply *))))

(->> (for [x (range side)
           y (range side)]
       (scenic-score [x y]))
     (apply max))