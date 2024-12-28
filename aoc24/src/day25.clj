(ns aoc24.src.day25
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [transpose]]
   [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn parse
  [input]
  (->> input
       (partition 8 8 (repeat ""))
       (map #(drop-last 1 %))
       (map transpose)
       (sort-by first)
       (split-with #(str/starts-with? (first %) "#"))))

(defn heights
  [key]
  (->> key
       (map (partial filter #(= % \#)))
       (map count)
       (map dec)))

(defn fits?
  [h1 h2]
  (->> (map + h1 h2)
       (remove #(<= % 5))
       (empty?)))

;; Part 1
(let [[keys locks] (parse real-input)]
  (->> (for [key keys
             lock locks]
         (fits? (heights key) (heights lock)))
       (filter true?)
       (count)
       #_(submit-answer 1)))
