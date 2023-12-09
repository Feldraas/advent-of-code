(ns aoc22.src.day2
  (:require
    [aoc-tools :refer [read-input]]
    [clojure.set :refer [map-invert]]))

(defn parse-strat
  [[opp-shape _ my-shape]]
  [(get {\A :rock \B :paper \C :scissors} opp-shape)
   (get {\X :rock \Y :paper \Z :scissors} my-shape)])

(def strats (read-input))

(def to-beat {:rock :paper :paper :scissors :scissors :rock})
(defn beats?
  [opp-shape my-shape]
  (= (get to-beat my-shape) opp-shape))

(defn shape-score
  [[_ my-shape]]
  (get {:rock 1 :paper 2 :scissors 3} my-shape))

(defn outcome-score
  [[opp-shape my-shape]]
  (cond
    (= my-shape opp-shape) 3
    (beats? my-shape opp-shape) 6
    :else 0))

(defn score
  [strat]
  (+ (shape-score strat) (outcome-score strat)))

(apply + (map score (map parse-strat strats)))

(defn parse-strat-correctly
  [[opp-shape-char _ desired-outcome]]
  (let [opp-shape (get {\A :rock \B :paper \C :scissors} opp-shape-char)]
    [opp-shape
     (cond
       (= desired-outcome \X) (get (map-invert to-beat) opp-shape)
       (= desired-outcome \Z) (get to-beat opp-shape)
       :else opp-shape)]))

(apply + (map score (map parse-strat-correctly strats)))