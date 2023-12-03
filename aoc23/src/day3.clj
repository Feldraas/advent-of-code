(ns aoc23.src.day3
  (:require [utils :refer [read-input
                           get-adjacent
                           parse-int]]))

(def schematic (-> (read-input)))
(def borders [(count schematic) (count (first schematic))])

(defn parse-char
  [c]
  (if (<= 48 (int c) 57)
    (str (- (int c) 48))
    (str c)))

(defn intstring?
  [s]
  (re-seq #"^\d+$" (str s)))

(defn merge-left
  [grid [x y]]
  (let [this (get grid [x y])
        left (get grid [x (dec y)])]
    (if (intstring? left)
      (-> grid
          (dissoc [x y])
          (assoc [x (dec y)] (str left this)))
      grid)))

(def raw-grid
  (as-> (for [x (range (first borders))
              y (range (second borders))
              :let [c (parse-char (nth (nth schematic x) y))]
              :when (not= c ".")]
          [[x y] c]) res
        (into {} res)))

(def merged-grid
  (let [coords (->> raw-grid
                    (filter #(intstring? (second %)))
                    (keys)
                    (sort-by (juxt first #(- (second %)))))]
    (reduce merge-left raw-grid coords)))



(defn get-neighbors
  [grid [x y] length]
  (map #(get grid %) (for [dx [-1 0 1]
                           dy (range -1 (inc length))]
                       [(+ x dx) (+ y dy)])))

(defn part?
  [grid [x y]]
  (let [num (get grid [x y])
        length (count (str num))
        neighbors (filter some? (get-neighbors grid [x y] length))]
    (and
      (intstring? num)
      (some #(not (intstring? %)) neighbors))))

(->> merged-grid
     (filter #(part? merged-grid (first %)))
     (sort-by first)
     (map second)
     (map parse-int)
     (apply +))

(defn read-right
  [grid [x y]]
  (let [this (get grid [x y])
        right (get grid [x (inc y)])]
    (if (intstring? right)
      (str this (read-right grid [x (inc y)]))
      this)))


(defn read-number
  [grid [x y]]
  (if (intstring? (get grid [x y]))
    (if (intstring? (get grid [x (dec y)]))
      (read-number grid [x (dec y)])
      (read-right grid [x y]))))

(defn get-adjacent-numbers
  [grid [x y]]
  (let [top (map #(read-number grid %) [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]])
        mid (map #(read-number grid %) [[x (dec y)] [x (inc y)]])
        bot (map #(read-number grid %) [[(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])]
    (filter some? (concat (if (second top) [(second top)] top)
                          mid
                          (if (second bot) [(second bot)] bot)))))

(defn gear?
  [grid point]
  (and
    (= (get grid point) "*")
    (= (count (get-adjacent-numbers grid point)) 2)))

(defn gear-ratio
  [grid point]
  (->> point
       (get-adjacent-numbers grid)
       (map parse-int)
       (apply *)))

(->> (keys raw-grid)
     (filter #(gear? raw-grid %))
     (sort)
     (map #(gear-ratio raw-grid %))
     (apply +))