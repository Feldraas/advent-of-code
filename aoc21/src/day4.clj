(ns aoc21.src.day4
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers transpose iterate-until]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))


(defn create-state
  [input]
  (let [[draws & boards] (remove #{""} input)]
    {:draws       (extract-numbers draws)
     :boards      (->> boards
                       (map #(vec (extract-numbers %)))
                       (partition 5)
                       (map #(hash-map :rows %)))
     :last-called nil}))

(defn winner?
  [board]
  (let [rows (:rows board)
        cols (transpose rows)]
    (some #(every? nil? %) (concat rows cols))))

(defn mark-on-board
  [board number]
  (update board :rows #(map (fn [row] (replace {number nil} row)) %)))

(defn mark-all
  [state]
  (let [number (first (:draws state))]
    (-> state
        (update :boards (fn [board] (map #(mark-on-board % number) board)))
        (update :boards #(sort-by winner? %))
        (update :draws rest)
        (assoc :last-called number))))

(defn final-score
  [input]
  (let [final-state   (->> input
                           create-state
                           (iterate-until #(some winner? (:boards %)) mark-all))
        winning-board (first (filter winner? (:boards final-state)))]
    (->> winning-board
         :rows
         flatten
         (remove nil?)
         (apply +)
         (* (:last-called final-state)))))

(->> real-input
     final-score
     time
     #_(submit-answer 1))

(let [final-state (->> real-input
                       create-state
                       (iterate mark-all)
                       (take-while #(not (every? winner? (:boards %))))
                       last)
      last-called (first (:draws final-state))]
  (->> final-state
       :boards
       (remove winner?)
       first
       :rows
       flatten
       (remove nil?)
       (apply + (- last-called))
       (* last-called)
       time
       #_(submit-answer 2)))

