(ns aoc24.src.day05
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [extract-numbers]]
   [clojure.set :as setfn]))


(def input (read-input :test))
(def real-input (read-input))


(defn get-rules
  [input]
  (->> input
       (split-with (partial not= ""))
       (first)
       (map extract-numbers)))

(defn get-updates
  [input]
  (->> input
       (split-with (partial not= ""))
       (second)
       (rest)
       (map extract-numbers)))

(defn correct-for-rule?
  [upd rule]
  (if (nil? upd)
    true
    (if (= (last upd) (first rule))
      (->> upd
           (butlast)
           (filter #{(second rule)})
           (empty?))
      (correct-for-rule? (butlast upd) rule))))

(defn correct?
  [upd rules]
  (if (empty? rules)
    true
    (if-not (correct-for-rule? upd (first rules))
      false
      (correct? upd (rest rules)))))

(defn middle
  [coll]
  (nth coll (/ (dec (count coll)) 2)))


;; Part 1
(let [rules (get-rules real-input)
      updates (get-updates real-input)]
  (->> updates
       (filter #(correct? % rules))
       (map middle)
       (apply +)
       #_(submit-answer 1)))

(defn relevant?
  [rule upd]
  (setfn/subset? (set rule) (set upd)))

(defn get-relevant-rules
  [upd rules]
  (filter #(relevant? % upd) rules))

(defn merge-rules
  [rules]
  (let [uniques (set (flatten rules))
        right-numbers (->> rules
                           (map second)
                           (set))
        leftmost (->> uniques
                      (remove right-numbers)
                      (first))
        rules-without-first (remove #(some #{leftmost} %) rules)]
    (if (= (count right-numbers) 1)
      (conj (list (first right-numbers)) leftmost)
      (conj (merge-rules rules-without-first) leftmost))))

(defn fix
  [upd rules]
  (let [relevant-rules (get-relevant-rules upd rules)
        merged-rules (merge-rules relevant-rules)]
    (filter (set upd) merged-rules)))


;; Part 2
(let [rules (get-rules real-input)
      updates (get-updates real-input)]
  (->> updates
       (remove #(correct? % rules))
       (map #(fix % rules))
       (map middle)
       (apply +)
       #_(submit-answer 2)))
