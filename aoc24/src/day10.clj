(ns aoc24.src.day10
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [get-adjacent mfilter mremove nmap]]
   [clojure.set :refer [union]]))

(def input (read-input :test))
(def real-input (read-input))


(defn get-heights
  [input]
  (into {}
        (for [x (range (count input))
              y (range (count (first input)))]
          [[x y] (read-string (str (nth (nth input x) y)))])))

(defn reachable-nines
  [pos heights]
  (let [h (get heights pos)]
    (cond
      (nil? h) #{}
      (= 9 h) #{pos}
      :else (->> pos
                 (get-adjacent 4)
                 (filter #(= (get heights %) (inc h)))
                 (map #(reachable-nines % heights))
                 (apply union)))))

;; Part 1
(let [heights (get-heights real-input)]
  (->> heights
       (mfilter :vals #{0})
       (keys)
       (map #(reachable-nines % heights))
       (map count)
       (apply +)
       #_(submit-answer 1)))

(defn trails
  [pos heights]
  (let [h (get heights pos)]
    (cond
      (nil? h) (list)
      (= 9 h) (list (list pos))
      :else (->> pos
                 (get-adjacent 4)
                 (filter #(= (get heights %) (inc h)))
                 (map #(trails % heights))
                 (nmap 2 #(conj % pos))
                 (apply concat)))))

;; Part 2
(let [heights (get-heights real-input)]
  (->> heights
       (mfilter :vals #{0})
       (keys)
       (map #(trails % heights))
       (apply concat)
       (map #(vector (first %) (last %)))
       #_(set) ;; Include for Part 1
       (count)
       #_(submit-answer 2)))

