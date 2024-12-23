(ns aoc24.src.day02
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [clojure.string :as str]
   [utils :refer [parse-int nmap]]))

(def input (read-input :test))
(def real-input (read-input))

(defn safe?
  [coll]
  (let [diffs (map - coll (rest coll))]
    (or
     (every? #{1 2 3} diffs)
     (every? #{-1 -2 -3} diffs))))

;; Part 1
(->> real-input
     (map #(str/split % #" "))
     (nmap 2 parse-int)
     (filter safe?)
     (count)
     #_(submit-answer 1))

(defn drop-nth
  [n coll]
  (concat
   (take n coll)
   (drop (inc n) coll)))

(defn dampened-safe?
  [coll]
  (if (safe? coll)
    true
    (->> coll
         (count)
         (range)
         (map #(drop-nth % coll))
         (some safe?))))

;; Part 2
(->> real-input
     (map #(str/split % #" "))
     (nmap 2 parse-int)
     (filter dampened-safe?)
     (count)
     #_(submit-answer 2))
