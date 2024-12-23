(ns aoc24.src.day08
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [clojure.set :refer [union]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-antennas
  [input]
  (into {} (for [x (range (count input))
                 y (range (count input))
                 :let [char (nth (nth input x) y)]
                 :when (not= char \.)] [[x y] char])))

(defn in-grid?
  [size [x y]]
  (<= 0 (min x y) (max x y) (dec size)))

(defn get-line
  [pos step grid-size]
  (let [forward (take-while
                 #(in-grid? grid-size %)
                 (iterate #(map + % step) pos))
        backward (take-while
                  #(in-grid? grid-size %)
                  (iterate #(map - % step) pos))]
    (->> (concat forward backward)
         (set))))


(defn find-nodes
  [antennas size part]
  (for [a1 (keys antennas)
        a2 (keys antennas)
        :let [diff (map - a1 a2)
              node-1 (map + a1 diff)
              node-2 (map - a2 diff)]
        :when (and
               (not= a1 a2)
               (= (get antennas a1) (get antennas a2)))]
    (if (= part 1)
      [node-1 node-2]
      (get-line a1 diff size))))

;; Part 1
(let [input input
      size (count input)]
  (->> input
       (get-antennas)
       (#(find-nodes % size 1))
       (flatten)
       (partition 2)
       (set)
       (filter #(in-grid? size %))
       (count)
       #_(submit-answer 1)))


;; Part 2
(let [input real-input
      size (count input)]
  (->> input
       (get-antennas)
       (#(find-nodes % size 2))
       (apply union)
       (filter #(in-grid? size %))
       (count)
       #_(submit-answer 2)))
