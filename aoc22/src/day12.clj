(ns aoc22.src.day12
  (:require [utils :refer [read-input
                           parse-int]]
            [clojure.string :as str]))

; TBD

;(def state {:grid        (read-input "test.txt")
;            :unavailable []
;            :checked     {}})
;
;(def borders
;  [(count (:grid state)) (count (first (:grid state)))])
;
;(defn valid?
;  [state [x y]]
;  (and
;    (not (in? (:unavailable state) [x y]))
;    (>= x 0)
;    (>= y 0)
;    (< x (first borders))
;    (< y (last borders))))
;
;(defn get-height
;  [state [x y]]
;  (let [height (nth (nth (:grid state) x) y)]
;    (get {\S \a \E \z} height height)))
;
;(defn get-entrances
;  [state [x y]]
;  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
;       (filter #(valid? state %))
;       (filter #(>= (- (int (get-height state %)) (int (get-height state [x y]))) -1))))
;
;
;(defn shortest-path
;  [state from-point to-point]
;  (do (println "Checking path from" from-point "to" to-point)
;      (let [entrances (get-entrances state to-point)]
;        (cond
;          (in? entrances from-point) 1
;          :else (let [new-state (update state :unavailable concat [entrances to-point])
;                      length (->> (map #(shortest-path new-state from-point %) entrances)
;                                  (map inc)
;                                  (sort)
;                                  (first))]
;                  (if (nil? length)
;                    999999
;                    length))))))
;
;
;(shortest-path state [0 0] [2 5])