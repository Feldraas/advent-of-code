(ns aoc22.src.day15
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]))

(def report (read-input))

(defn distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def zone (as-> report $
                (map #(re-seq #"-?\d+" %) $)
                (map #(map parse-int %) $)
                (map #(partition 2 %) $)
                (reduce #(let [sensor (first %2)
                               beacon (second %2)]
                           (-> %1
                               (assoc-in [:grid sensor] "S")
                               (assoc-in [:grid beacon] "B")
                               (assoc-in [:distances sensor] (distance sensor beacon))))
                        {} $)))



(def borders
  (as-> (:grid zone) $
        (keys $)
        [(map first $) (map second $)]
        (map (juxt #(apply min %) #(apply max %)) $)))

(def xmin (first (first borders)))
(def xmax (second (first borders)))
(def ymin (first (second borders)))
(def ymax (second (second borders)))

(defn draw-zone
  ([zone]
   (draw-zone zone 0))
  ([zone padding]
   (->> (for [y (range 0 21)
              x (range 0 21)]
          (get-in zone [:grid [x y]] "."))
        (partition 21)
        (map #(apply str %)))))

;(draw-zone zone)


(defn in-radius
  [r [x y]]
  (for [dx (range (- r) (inc r))
        dy (range (- (abs dx) r) (- (inc r) (abs dx)))
        :when (not= [dx dy] [0 0])]
    [(+ x dx) (+ y dy)]))


(defn in-radius2
  [r [x y] row]
  (let [diff (abs (- y row))]
    (for [dx (range (- diff r) (inc (- r diff)))]
      [(+ x dx) row])))

(defn eliminate-for-sensor
  [zone sensor row]
  (reduce #(update-in %1 [:grid %2] (fn [x] (or x "#")))
          zone
          (in-radius2 (get-in zone [:distances sensor]) sensor row)))

(defn eliminate-all
  [zone row]
  (reduce #(eliminate-for-sensor %1 %2 row) zone (keys (:distances zone))))


(def row 1)
(time (def zzone (eliminate-all zone row)))
(time (->> (for [x (range 4000000)]
             [x row])
           (remove #(get-in zzone [:grid %]))
           (count)))