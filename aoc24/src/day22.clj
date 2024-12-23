(ns aoc24.src.day22
  (:require
   [aoc-tools :refer [read-input submit-answer]]))

(def input (map read-string (read-input :test)))
(def real-input (map read-string (read-input)))


(defn step-1
  [secret]
  (-> secret
      (* 64)
      (bit-xor secret)
      (mod 16777216)))

(defn step-2
  [secret]
  (-> secret
      (/ 32)
      (int)
      (bit-xor secret)
      (mod 16777216)))

(defn step-3
  [secret]
  (-> secret
      (* 2048)
      (bit-xor secret)
      (mod 16777216)))

(defn calc-next
  [secret]
  ((comp step-3 step-2 step-1) secret))


;; Part 1
(->> real-input
     (map (partial iterate calc-next))
     (map (partial take 2001))
     (map last)
     (apply +)
     #_(submit-answer 1))


(defn prices
  [secret]
  (->> secret
       (iterate calc-next)
       (map #(mod % 10))))

(defn price-diffs
  [secret]
  (let [prices (prices secret)]
    (map - (drop 1 prices) prices)))

(defn price-map
  [secret]
  (let [price-seq (take 2001 (prices secret))
        diff-seq (take 2000 (price-diffs secret))]
    (->> (for [x (range 4 2001)]
           [(take 4 (drop (- x 4) diff-seq)) (nth price-seq x)])
         (reverse)
         (into {}))))

(defn total-bananas
  [diffs pmaps]
  (apply + (map #(get % diffs 0) pmaps)))

;; Part 2
(time
 (let [pmaps (map price-map real-input)]
   (->> pmaps
        (map keys)
        (apply concat)
        (set)
        (map #(total-bananas % pmaps))
        (apply max)
        #_(submit-answer 2))))
