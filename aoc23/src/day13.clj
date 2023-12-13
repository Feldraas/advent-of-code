(ns aoc23.src.day13
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [indices]]
    [clojure.set :refer [map-invert]]))

(def input (->> (read-input :test)
                (partition-by #{""})
                (remove #{(list "")})))
(def real-input (->> (read-input)
                     (partition-by #{""})
                     (remove #{(list "")})))

(defn can-reflect-at-index?
  [string idx]
  (let [[left-split right-split] (split-at (inc idx) string)
        size (min (count left-split) (count right-split))
        left (take-last size left-split)
        right (take size right-split)]
    (= left (reverse right))))

(defn possible-reflections
  [string]
  (->> (indices string)
       (filter #(can-reflect-at-index? string %))))

(defn col-score
  [pattern]
  (let [freqs (->> pattern
                   (map possible-reflections)
                   flatten
                   frequencies
                   (map-invert))
        regular-count (get freqs (count pattern) -1)
        smudge-count (get freqs (dec (count pattern)) -1)]
    (->> [regular-count smudge-count]
         (map inc))))

(defn row-score
  [pattern]
  (let [transpose (->> pattern
                       (apply mapv vector)
                       (map #(apply str %)))]
    (->> (col-score transpose)
         (map #(* 100 %)))))

(defn total-scores
  [input]
  (->> input
       (map #(map + (col-score %) (row-score %)))
       (reduce (partial map +))))

(->> real-input
     total-scores
     first
     time
     #_(submit-answer 1))

(->> real-input
     total-scores
     second
     time
     #_(submit-answer 2))
