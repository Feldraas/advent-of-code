(ns aoc23.src.day2
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]
    [clojure.string :as str]))

(defn parse-color
  [color string]
  (as-> color $
        (str "(\\d+) " $)
        (re-pattern $)
        (re-seq $ string)
        (if (nil? $)
          0
          (parse-int (last (first $))))))

(defn parse-round
  [round]
  (map #(parse-color % round) ["red" "green" "blue"]))

(defn parse-game
  [game]
  (as-> game $
        (str/split $ #";")
        (map parse-round $)))

(defn round-possible?
  [game limits]
  (and
    (<= (first game) (first limits))
    (<= (second game) (second limits))
    (<= (last game) (last limits))))

(defn game-possible?
  [game limits]
  (->> game
       (map #(round-possible? % limits))
       (every? true?)))


(def games (read-input))
(def cube-limits [12 13 14])

(time (as-> games $
            (map parse-game $)
            (map #(game-possible? % cube-limits) $)
            (filter #(nth $ (dec %)) (range 1 (inc (count games))))
            (apply + $)))

(defn minimum-amounts
  [game]
  (reduce #(list (max (first %1) (first %2))
                 (max (second %1) (second %2))
                 (max (last %1) (last %2))) game))

(time (->> games
           (map parse-game)
           (map minimum-amounts)
           (map #(apply * %))
           (apply +)))