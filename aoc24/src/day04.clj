(ns aoc24.src.day04
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [non-neg?]]))

(def input (read-input :test))
(def real-input (read-input))

(defn valid-position?
  [pos input]
  (and
   (< (first pos) (count input))
   (< (second pos) (count input))
   (non-neg? (first pos))
   (non-neg? (second pos))))

(defn char-at
  [pos input]
  (if (valid-position? pos input)
    (nth (nth input (first pos)) (second pos))
    \.))


(defn xmas-in-direction?
  [pos-1 direction input]
  (let [pos-2 (map + pos-1 direction)
        pos-3 (map + pos-2 direction)
        pos-4 (map + pos-3 direction)]
    (and
     (= (char-at pos-1 input) \X)
     (= (char-at pos-2 input) \M)
     (= (char-at pos-3 input) \A)
     (= (char-at pos-4 input) \S))))

(defn xmas-count
  [pos input]
  (let [directions [[0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]]]
    (->> directions
         (filter #(xmas-in-direction? pos % input))
         (count))))


(defn total-xmas-count
  [input]
  (let [coords (for [x (range (count input))
                     y (range (count input))]
                 [x y])]
    (->> coords
         (map #(xmas-count % input))
         (apply +))))

;; Part 1
(->> real-input
     (total-xmas-count)
     #_(submit-answer 1))

(defn mas-cross?
  [pos input]
  (let [directions [[1 1] [-1 -1] [1 -1] [-1 1]]
        neighbors (->> directions
                       (map #(map + pos %))
                       (map #(char-at % input)))]
    (and
     (= (char-at pos input) \A)
     (= (apply str (sort neighbors)) "MMSS")
     (not= (apply str neighbors) "MMSS")
     (not= (apply str neighbors) "SSMM"))))

(defn total-mas-cross-count
  [input]
  (let [coords (for [x (range (count input))
                     y (range (count input))]
                 [x y])]
    (->> coords
         (filter #(mas-cross? % input))
         (count))))

;; Part 2
(->> real-input
     (total-mas-cross-count)
     #_(submit-answer 2))
