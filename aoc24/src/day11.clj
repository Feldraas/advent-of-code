(ns aoc24.src.day11
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [extract-numbers ->! ->>! mremove]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-stones
  [input]
  (->> input
       (first)
       (extract-numbers)
       (->! interleave (repeat 1))
       (partition 2)
       (map vec)
       (into {})))

(defn safe
  [x f by]
  (if (nil? x)
    by
    (f x by)))

(defn change
  [stones [stone curr-count]]
  (let [num-digits (count (str stone))]
    (cond
      (zero? stone) (-> stones
                        (update stone safe - curr-count)
                        (update 1 safe + curr-count))
      (even? num-digits) (let [s (str stone)
                               len (count s)
                               [left right] (->> s
                                                 (partition (/ len 2))
                                                 (map (partial apply str))
                                                 (map extract-numbers)
                                                 (map first))]
                           (-> stones
                               (update stone safe - curr-count)
                               (update left safe + curr-count)
                               (update right safe + curr-count)))
      :else (-> stones
                (update stone safe - curr-count)
                (update (* stone 2024) safe + curr-count)))))

(defn blink
  [stones]
  (reduce change stones stones))

;; Part 1 and 2
(->> real-input
     (get-stones)
     (iterate blink)
     (drop 1) ; Initial
     (take 75)
     (last)
     (vals)
     (apply +)
     #_(submit-answer 2))
