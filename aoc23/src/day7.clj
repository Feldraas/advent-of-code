(ns aoc23.src.day7
  (:require [utils :refer [read-input parse-int nmap in?]]
            [ysera.test :refer [is is-not is=]]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.core.match :refer [match]]))

(def test-hands (->> (read-input :test)
                     (map #(str/split % #" "))
                     (into {})
                     (#(update-vals % parse-int))))

(def real-hands (->> (read-input)
                     (map #(str/split % #" "))
                     (into {})
                     (#(update-vals % parse-int))))

(defn get-card-strength
  [card jokers?]
  (if (and jokers?
           (= card \J))
    -1
    (get (zipmap (map char "23456789TJQKA") (range 13)) card)))

(defn type-strength
  [hand jokers?]
  (let [joker-count (count (re-seq #"J" hand))
        other-cards (->> hand
                         (#(str/replace % "J" ""))
                         (frequencies)
                         (vals)
                         (sort))]
    (if (and jokers? (pos? joker-count))
      (case [joker-count other-cards]
        [1 [1 1 1 1]] [:one-pair 2]
        [1 [1 1 2]] [:three-of-a-kind 4]
        [1 [1 3]] [:four-of-a-kind 6]
        [1 [2 2]] [:full-house 5]
        [1 [4]] [:five-of-a-kind 7]
        [2 [1 1 1]] [:three-of-a-kind 4]
        [2 [1 2]] [:four-of-a-kind 6]
        [2 [3]] [:five-of-a-kind 7]
        [3 [1 1]] [:four-of-a-kind 6]
        [3 [2]] [:five-of-a-kind 7]
        [4 [1]] [:five-of-a-kind 7]
        [5 []] [:five-of-a-kind 7]
        (type-strength hand jokers?))

      (case
        [5] [:five-of-a-kind 7]
            [1 4] [:four-of-a-kind 6]
            [2 3] [:full-house 5]
            [1 1 3] [:three-of-a-kind 4]
            [1 2 2] [:two-pair 3]
            [1 1 1 2] [:one-pair 2]
            [1 1 1 1 1] [:high-card 1]))))

(defn compare-first-card
  [left right jokers?]
  (let [left-strength (get-card-strength (first left) jokers?)
        right-strength (get-card-strength (first right) jokers?)]
    (cond
      (< left-strength right-strength) -1
      (> left-strength right-strength) +1
      :else (compare-first-card (rest left) (rest right) jokers?))))

(defn compare-hands
  [left right jokers?]
  (let [left-strength (second (type-strength left jokers?))
        right-strength (second (type-strength right jokers?))]
    (cond
      (< left-strength right-strength) -1
      (> left-strength right-strength) +1
      :else (compare-first-card left right jokers?))))

(defn winnings
  [hands & [jokers?]]
  (->> hands
       (sort-by first #(compare-hands %1 %2 jokers?))
       (map second)
       (map vector (range 1 (inc (count hands))))
       (map #(apply * %))
       (apply +)))

(winnings real-hands)
(winnings real-hands :jokers)






