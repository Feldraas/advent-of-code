(ns aoc23.src.day7
  (:require [utils :refer [read-input parse-int ->!]]
            [clojure.string :as str]))

(def test-input (read-input :test))
(def real-input (read-input))

(defn get-hands-and-bids
  [input jokers?]
  (as-> input res
        (map #(str/split % #" ") res)
        (into {} res)
        (update-vals res parse-int)
        (if jokers?
          (update-keys res #(str/replace % "J" "X"))
          res)))

(defn strength
  [hand]
  (let [joker-count (count (re-seq #"X" hand))
        other-cards (remove (partial = \X) hand)
        top-2 (->> other-cards
                   (frequencies)
                   (vals)
                   (->! conj 0 0)
                   (sort >)
                   (take 2))]
    [(+ joker-count (first top-2)) (second top-2)]))

(defn compare-by-first-card
  [left-hand right-hand]
  (let [value-map (zipmap (map char "X23456789TJQKA") (range 14))
        left-card-value (get value-map (first left-hand))
        right-card-value (get value-map (first right-hand))
        comp (compare left-card-value right-card-value)]
    (if (zero? comp)
      (compare-by-first-card (rest left-hand) (rest right-hand))
      comp)))

(defn compare-hands
  [left-hand right-hand]
  (let [left-hand-strength (strength left-hand)
        right-hand-strength (strength right-hand)]
    (if (= left-hand-strength right-hand-strength)
      (compare-by-first-card left-hand right-hand)
      (compare left-hand-strength right-hand-strength))))

(defn winnings
  [input & [jokers?]]
  (->> (get-hands-and-bids input jokers?)
       (sort-by first compare-hands)
       (map second)
       (map vector (range 1 (inc (count input))))
       (map #(apply * %))
       (apply +)))

(time (winnings real-input))
(time (winnings real-input :jokers))