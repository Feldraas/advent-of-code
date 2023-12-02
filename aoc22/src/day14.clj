(ns aoc22.src.day14
  (:require [utils :refer [read-input
                           parse-int]]
            [clojure.string :as str]))

(def wall-char "▓")
(def sand-char ".")
(def air-char " ")
(def source-char "+")

(defn wall-range
  [start end]
  (if (<= start end)
    (range start (inc end))
    (range end (inc start))))

(defn add-wall
  [cave start end]
  (reduce #(assoc-in %1 [:grid %2] wall-char)
          cave
          (for [x (wall-range (first start) (first end))
                y (wall-range (second start) (second end))]
            [x y])))

(defn add-walls
  [cave points]
  (reduce #(add-wall %1 (nth points %2) (nth points (inc %2)))
          cave
          (drop-last 1 (range (count points)))))

(def walls (->> (read-input "day14.txt")
                (map #(str/replace % #"(,| -> )" " "))
                (map #(str/split % #" "))
                (map #(map parse-int %))
                (map #(partition 2 %))
                (map vec)))

(def source [500 0])

(def empty-cave {:current-position source
                 :current-state    :falling
                 :grid             {source source-char}})

(def cave (reduce add-walls empty-cave walls))

(def borders
  (as-> (:grid cave) $
        (keys $)
        [(map first $) (map second $)]
        (map (juxt #(apply min %) #(apply max %)) $)))

(def xmin (first (first borders)))
(def xmax (second (first borders)))
(def ymax (second (second borders)))

(defn free?
  [cave point]
  (and (<= (second point) (inc ymax))
       (= (get-in cave [:grid point] air-char) air-char)))

;(defn free-fall?
;  [cave point]
;  (and (free? cave point)
;       (->> (union (:walls cave) (:sand cave))
;            (filter #(and (= (first %) (first point))
;                          (> (second %) (second point))))
;            (empty?))))

(defn force-down
  [cave [x y]]
  (if (free? cave [x (inc y)])
    (force-down cave [x (inc y)])
    [x y]))

(defn update-cave
  [cave]
  (let [[x y] (force-down cave (:current-position cave))
        left [(dec x) (inc y)]
        right [(inc x) (inc y)]
        next (first (filter #(free? cave %) [left right]))]
    (cond
      (nil? next) (-> cave
                      (assoc-in [:grid [x y]] sand-char)
                      (assoc :current-position source))
      ;(free-fall? cave next) (-> cave
      ;                           (assoc :current-state :free-fall))
      :else (assoc cave :current-position next))))

(defn draw-cave
  ([cave]
   (draw-cave cave 5))
  ([cave padding]
   (->> (for [y (range 0 (+ 3 ymax))
              x (range (- xmin padding) (+ xmax padding))]
          (if (= y (+ 2 ymax))
            wall-char
            (get-in cave [:grid [x y]] air-char)))
        (partition (+ (* 2 padding) (- xmax xmin)))
        (map #(apply str %)))))

(defn draw-nth
  [n]
  (draw-cave (nth (iterate update-cave cave) n)))

(def final-cave (->> (take-while #(not= (get-in % [:grid source]) sand-char) (iterate update-cave cave))
                     (last)
                     (update-cave)))

(->> final-cave
     (:grid)
     (vals)
     (filter #(= sand-char %))
     (count))

;(spit "image.txt" (str/join "\n" (draw-cave final-cave 200)))