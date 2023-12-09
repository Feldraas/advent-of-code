(ns aoc22.src.day13
  (:require
    [aoc-tools :refer [read-input]]
    [clojure.string :as str]))

(defn wrap-first
  [coll]
  (list (list (first coll)) (rest coll)))

(defn ordered?
  [left right]
  (cond

    (and (nil? left) (nil? right)) true

    (and (empty? left) (empty? right)) true

    (and (int? (first left)) (list? (first right)))
    (ordered? (wrap-first left) right)

    (and (list? (first left)) (int? (first right)))
    (ordered? left (wrap-first right))

    (= (first left) (first right))
    (ordered? (rest left) (rest right))

    (and (int? (first left)) (int? (first right)))
    (cond
      (< (first left) (first right)) true
      (> (first left) (first right)) false
      :else (ordered? (rest left) (rest right)))

    (and (empty? left) (not-empty right)) true
    (and (not-empty left) (empty? right)) false

    :else (ordered? (first left) (first right))))

(def packets (->> (read-input)
                  (remove empty?)
                  (map #(str/replace % "[" "("))
                  (map #(str/replace % "]" ")"))
                  ;(partition 2)
                  (map read-string)))

(def packets-with-dividers (conj packets '((2)) '((6))))

(->> (sort ordered? packets-with-dividers)
     (zipmap (range (count packets-with-dividers)))
     (filter #(or (= (second %) '((2)))
                  (= (second %) '((6)))))
     (map first)
     (map inc)
     (apply *))