(ns aoc22.src.day11
  (:require [utils :refer [read-input
                           parse-int]]
            [clojure.string :as str]))

(def raw (read-input "day11.txt"))
(def num-monkeys (->> (drop-last 5 raw)
                      (last)
                      (drop-last 1)
                      (last)
                      (int)
                      (+ -47)))
(def state
  (apply merge (for [i (range num-monkeys)]
                 {i {:id              i
                     :inspected       0
                     :items           (-> (nth raw (+ 1 (* i 7)))
                                          (str/split #": ")
                                          (last)
                                          (str/split #", ")
                                          ((partial map parse-int))
                                          (vec))
                     :worry-fn        (as-> (nth raw (+ 2 (* i 7))) $
                                            (str/split $ #"old ")
                                            (last $)
                                            (str/split $ #" ")
                                            (let [self? (= (last $) "old")
                                                  op (if (= (first $) "+") + *)]
                                              (if self?
                                                #(op % %)
                                                #(op % (parse-int (last $))))))
                     :divisor         (-> (nth raw (+ 3 (* i 7)))
                                          (str/split #" ")
                                          (last)
                                          (parse-int))
                     :monkey-if-true  (-> (nth raw (+ 4 (* i 7)))
                                          (str/split #" ")
                                          (last)
                                          (parse-int))
                     :monkey-if-false (-> (nth raw (+ 5 (* i 7)))
                                          (str/split #" ")
                                          (last)
                                          (parse-int))}})))

(def prime-product (->> state
                        (vals)
                        (map :divisor)
                        (apply *)))


(defn inspect-item-for-monkey
  [state monkey]
  (let [start-worry (first (get-in state [monkey :items]))
        worry-fn (get-in state [monkey :worry-fn])
        divisor (get-in state [monkey :divisor])
        final-worry (-> (worry-fn start-worry)
                        (mod prime-product)
                        (int))
        receiver-monkey (if (zero? (mod final-worry divisor))
                          (get-in state [monkey :monkey-if-true])
                          (get-in state [monkey :monkey-if-false]))]
    (-> state
        (update-in [monkey :items] #(vec (rest %)))
        (update-in [monkey :inspected] inc)
        (update-in [receiver-monkey :items] conj final-worry))))

(defn inspect-all-items-for-monkey
  [state monkey]
  (let [num-items (count (get-in state [monkey :items]))]
    (nth (iterate #(inspect-item-for-monkey % monkey) state) num-items)))


(defn one-round
  [state]
  (reduce inspect-all-items-for-monkey state (range num-monkeys)))

(as-> (iterate one-round state) $
      (nth $ 10000)
      (vals $)
      (map :inspected $)
      (sort $)
      (drop (- num-monkeys 2) $)
      (apply * $))