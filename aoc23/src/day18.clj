(ns aoc23.src.day18
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [parse-int iterate-until]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn parse-steps
  [input part]
  (if (= part 1)
    (->> input
         (map #(str/split % #" "))
         (map drop-last)
         (map #(list (first %) (parse-int (second %)))))
    (->> input
         (map #(str/split % #" "))
         (map last)
         (map #(vector (str "0x" (subs % 2 7)) (subs % 7 8)))
         (map #(vector (get {"0" "R" "1" "D" "2" "L" "3" "U"} (second %)) (read-string (first %)))))))

(defn create-state
  ([input]
   (create-state input 1))
  ([input part]
   {:steps              (parse-steps input part)
    :center-coordinates [[0.5 0.5]]
    :area               0}))

(defn full-step
  [direction length]
  (map #(* % length) (get {"R" [0 1]
                           "L" [0 -1]
                           "U" [-1 0]
                           "D" [1 0]} direction)))

(defn take-step
  [{steps :steps centers :center-coordinates :as state}]
  (let [[direction length] (first steps)]
    (-> state
        (update :center-coordinates conj (map + (last centers) (full-step direction length)))
        (update :steps rest))))

(defn get-center-coordinates
  [state]
  (->> state
       (iterate-until #(empty? (:steps %)) take-step)
       :center-coordinates
       drop-last))

(defn normalize
  [vec]
  (map #(if (zero? %)
          %
          (/ % (abs %))) vec))

(def down [1.0 0.0])
(def up [-1.0 0.0])
(def left [0.0 -1.0])
(def right [0.0 1.0])

(def up-left [-0.5 -0.5])
(def up-right [-0.5 0.5])
(def down-left [0.5 -0.5])
(def down-right [0.5 0.5])

(defn shifts-at-idx
  [centers i]
  (let [curr     (nth centers i)
        next     (nth centers (mod (inc i) (count centers)))
        prev     (nth centers (mod (dec i) (count centers)))
        from-vec (normalize (map - curr prev))
        to-vec   (normalize (map - next curr))]
    (get {[up right]   up-left
          [up left]    up-right
          [down right] up-right
          [down left]  up-left
          [right up]   up-left
          [right down] up-right
          [left up]    up-right
          [left down]  up-left}
         [from-vec to-vec])))

(defn get-shifts
  [centers]
  (map #(shifts-at-idx centers %) (range (count centers))))

(defn get-vertices
  [state]
  (let [centers (get-center-coordinates state)
        shifts  (get-shifts centers)]
    (map (fn [c sh] [(map + c sh) (map - c sh)]) centers shifts)))

(defn get-loop
  [state n]
  (let [vertices (get-vertices state)
        steps    (:steps state)]
    (reduce (fn [acc-vec idx]
              (let [step            (nth steps idx)
                    last-coord      (last acc-vec)
                    next-coord-pair (nth vertices (inc idx))
                    connected-coord (case (first step)
                                      "R" (->> next-coord-pair
                                               (filter #(and
                                                          (= (first %) (first last-coord))
                                                          (> (second %) (second last-coord))))
                                               first)
                                      "L" (->> next-coord-pair
                                               (filter #(and
                                                          (= (first %) (first last-coord))
                                                          (< (second %) (second last-coord))))
                                               first)
                                      "U" (->> next-coord-pair
                                               (filter #(and
                                                          (< (first %) (first last-coord))
                                                          (= (second %) (second last-coord))))
                                               first)
                                      "D" (->> next-coord-pair
                                               (filter #(and
                                                          (> (first %) (first last-coord))
                                                          (= (second %) (second last-coord))))
                                               first))]
                (if connected-coord
                  (conj acc-vec connected-coord)
                  acc-vec)))
            [(if (= n 1) (first (first vertices)) (second (first vertices)))]
            (range (dec (count steps))))))

(defn loop-area
  [vertices]
  (loop [area     0
         vertices vertices]
    (if (= (count vertices) 1)
      (abs (/ area 2))
      (let [[x0 y0] (first vertices)
            [x1 y1] (second vertices)]
        (recur (+ area (- (* x0 y1) (* x1 y0))) (rest vertices))))))

(defn total-area
  [part input]
  (let [state (create-state input part)]
    (->> [1 2]
         (map #(get-loop state %))
         (map loop-area)
         (apply max))))

(->> real-input
     (total-area 1)
     int
     time
     #_(submit-answer 1))

(->> real-input
     (total-area 2)
     str
     (drop-last 3)
     (remove #{\.})
     (apply str)
     time
     #_(submit-answer 2))
