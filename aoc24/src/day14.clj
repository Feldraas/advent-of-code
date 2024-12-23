(ns aoc24.src.day14
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [extract-numbers]]))

(def input (read-input :test))
(def real-input (read-input))

(def borders [101 103])
(def hrz (/ (dec (first borders)) 2))
(def vrt (/ (dec (second borders)) 2))

(defn get-robots
  [input]
  (->> input
       (map extract-numbers)))

(defn move
  [[x y vx vy]]
  [(mod (+ x vx) (first borders)) (mod (+ y vy) (second borders)) vx vy])

(defn move-all
  [robots]
  (map move robots))

(defn quadrant
  [[x y vx vy]]
  (cond
    (and (< x hrz) (< y vrt)) 1
    (< x hrz) 2
    (< y vrt) 3
    :else 4))

;; Part 1
(->> real-input
     (get-robots)
     (iterate move-all)
     (drop 100)
     (first)
     (remove #(= (first %) hrz))
     (remove #(= (second %) vrt))
     (map quadrant)
     (frequencies)
     (vals)
     (apply *)
     #_(submit-answer 1))


(defn no-tree?
  [robots]
  (as-> robots res
    (map (partial take 2) res)
    (map vec res)
    (sort res)
    (map #(map - %1 %2) res (rest res))
    (partition-by #{[0 -1]} res)
    (filter #(every? #{[0 -1]} %) res)
    (map count res)
    (apply max res)
    (>= res 15)
    (not res)))

;; Part 2
(time
 (->> real-input
      (get-robots)
      (iterate move-all)
      (take-while no-tree?)
      (count)
      #_(submit-answer 2)))
