(ns aoc23.src.day4
  (:require [utils :refer [read-input parse-int]]
            [clojure.string :as str]
            [clojure.set :as cset]))

(def cards (read-input))

(defn score
  [card]
  (as-> (str/replace card #"Card +\d+: " "") $
        (str/split $ #" \| ")
        (map #(str/split % #" +") $)
        (let [[winners numbers] (map #(set (map parse-int %)) $)
              matches (count (cset/intersection winners numbers))]
          (if (zero? matches)
            0
            (reduce * (repeat (dec matches) 2))))))

(apply + (map score cards))

; Part 2

(defn matches
  [card]
  (as-> (str/replace card #"Card +\d+: " "") $
        (str/split $ #" \| ")
        (map #(str/split % #" +") $)
        (let [[winners numbers] (map #(set (map parse-int %)) $)]
          (count (cset/intersection winners numbers)))))

(def copies (zipmap (range 1 (inc (count cards))) (repeat 1)))

(defn process-card
  [copies n]
  (let [card (nth cards (dec n))
        num-matches (matches card)
        cards-to-add (range (inc n) (+ n num-matches 1))]
    (reduce #(update %1 %2 + (get %1 n)) copies cards-to-add)))

(as-> copies $
      (reduce #(process-card %1 %2) $ (sort (keys $)))
      (vals $)
      (apply + $))