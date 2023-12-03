(ns aoc22.src.day12
  (:require [utils :refer [read-input
                           in?]]
            [clojure.string :as str]))


(def grid (read-input "test.txt"))

(defn find-height
  [grid height]
  (as-> (map #(str/index-of % height) grid) $
        (zipmap (range (count $)) $)
        (filter #(int? (second %)) $)
        (first $)))

(def start (find-height grid "S"))
(def destination (find-height grid "E"))

(def state {:grid        grid
            :unavailable [start]
            :shortest    {[start destination] 999999}})

(def borders
  [(count (:grid state)) (count (first (:grid state)))])

(defn valid?
  [state [x y]]
  (and
    (not (in? (:unavailable state) [x y]))
    (>= x 0)
    (>= y 0)
    (< x (first borders))
    (< y (last borders))))

(defn get-height
  [state [x y]]
  (let [height (nth (nth (:grid state) x) y)]
    (get {\S \a \E \z} height height)))

(defn get-entrances
  [state [x y]]
  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
       (filter #(valid? state %))
       (filter #(>= (- (int (get-height state %)) (int (get-height state [x y]))) -1))))

(defn get-exits
  [state [x y]]
  (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
       (filter #(valid? state %))
       (filter #(>= (compare (get-height state [x y]) (get-height state %)) -1))))

(defn shortest-path
  [state start destination]
  (do (println "Checking from" start "to" destination)
      (let [exits (do (println start "has exits" (get-exits state start)) (get-exits state start))]
        (cond
          (in? (keys (:shortest state)) [start destination]) state
          (empty? exits) state
          (in? exits destination) (assoc-in state [:shortest [start destination]] 1)
          :else (let [new-state (update state :unavailable concat exits)
                      reduced-state (reduce #(shortest-path %1 %2 destination) new-state exits)]
                  (as-> exits $
                        (map #(get-in reduced-state [:shortest %] 999999) $)
                        (apply min $)
                        (inc $)
                        (assoc-in reduced-state [:shortest [start destination]] $)))))))

(shortest-path state start destination)