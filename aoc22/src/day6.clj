(ns aoc22.src.day6
  (:require [utils :refer [read-input]]))

(def message (first (read-input "day6.txt")))

(defn find-distinct
  ([n string]
   (find-distinct n string 0))
  ([n string rec-idx]
   (if
     (= (count (set (take n string))) n)
     (+ n rec-idx)
     (find-distinct n (apply str (rest string)) (inc rec-idx)))))

(find-distinct 4 message)
(find-distinct 14 message)