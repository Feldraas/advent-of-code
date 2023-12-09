(ns aoc23.src.day4
  (:require
    [utils :refer [read-input parse-int]]
    [clojure.string :as str]
    [clojure.set :refer [intersection]]))

(def cards (read-input))

(defn score
  [card]
  (as-> (str/replace card #"Card +\d+: " "") $
        (str/split $ #" \| ")
        (map #(str/split % #" +") $)
        (let [[winners numbers] (map #(set (map parse-int %)) $)
              matches (count (intersection winners numbers))]
          (if (zero? matches)
            0
            (reduce * (repeat (dec matches) 2))))))

(time (apply + (map score cards)))

; Part 2

(defn matches
  [card]
  (as-> (str/replace card #"Card +\d+: " "") $
        (str/split $ #" \| ")
        (map #(str/split % #" +") $)
        (let [[winners numbers] (map #(set (map parse-int %)) $)]
          (count (intersection winners numbers)))))

(def copies (zipmap (range 1 (inc (count cards))) (repeat 1)))

(defn process-card
  [copies n]
  (let [card (nth cards (dec n))
        num-matches (matches card)
        cards-to-add (range (inc n) (+ n num-matches 1))]
    (reduce (fn [acc-copies card]
              (update acc-copies card + (get acc-copies n)))
            copies
            cards-to-add)))

(time (as-> copies $
            (reduce process-card $ (sort (keys $)))
            (vals $)
            (apply + $)))