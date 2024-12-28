(ns aoc24.src.day24
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [->! mfilter]]
   [clojure.string :as str]))

(def input (read-input :day24.ex2))
(def real-input (read-input))

(defn get-initial-state
  [input]
  (->> input
       (filter #(str/includes? % ":"))
       (map #(str/split % #": "))
       (into {})
       (->! update-vals read-string)))

(defn get-instructions
  [input]
  (->> input
       (remove #(str/includes? % ":"))
       (drop 1)
       (interleave (range))
       (partition 2)
       (map vec)
       (into {})))

(defn get-state
  [input]
  (assoc (get-initial-state input) :instructions (get-instructions input)))

(defn parse-one
  [state instr-nr]
  (let [instr (get-in state [:instructions instr-nr])
        [left op right _ out] (str/split instr #" ")
        op (get {"AND" bit-and "OR" bit-or "XOR" bit-xor} op)]
    (if (and
         (state left)
         (state right))
      (-> state
          (assoc out (op (state left) (state right)))
          (update-in [:instructions] dissoc instr-nr))
      state)))

(defn parse-all
  [state]
  (if (empty? (state :instructions))
    (dissoc state :instructions)
    (reduce parse-one state (keys (state :instructions)))))

;; Part 1
(let [input real-input
      state (get-state input)
      instructions (get-instructions input)]
  (->> state
       (iterate parse-all)
       (drop-while :instructions)
       (first)
       (mfilter #(str/starts-with? % "z"))
       (sort)
       (map second)
       (map * (iterate #(* 2 %) 1))
       (apply +)
       #_(submit-answer 1)))
