(ns aoc22.src.day20
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int mfilter]]
    [ysera.test :refer [is=]]))


(def input (map parse-int (read-input :test)))
(def real-input (map parse-int (read-input)))

(defn create-state
  [input]
  (zipmap (range (count input))
          (for [i (range (count input))]
            {:orig-index    i
             :value         (nth input i)
             :steps-to-move (mod (nth input i) (dec (count input)))})))

(defn print-vec
  [state]
  (->> state
       sort
       (map second)
       (map :value)))

(defn move
  {:test (fn []
           (let [test-vec [9 4 6 6 5 -8 1 4 -5 -7]]
             (is= (-> test-vec create-state (move 0) print-vec)
                  [9 4 6 6 5 -8 1 4 -5 -7])
             (is= (-> test-vec create-state (move 1) print-vec)
                  [9 6 6 5 -8 4 1 4 -5 -7])
             (is= (-> test-vec create-state (move 2) print-vec)
                  [9 4 6 5 -8 1 4 -5 6 -7])
             (is= (-> test-vec create-state (move 3) print-vec)
                  [6 9 4 6 5 -8 1 4 -5 -7])
             (is= (-> test-vec create-state (move 4) print-vec)
                  [5 9 4 6 6 -8 1 4 -5 -7])
             (is= (-> test-vec create-state (move 5) print-vec)
                  [9 4 6 6 5 1 -8 4 -5 -7])
             (is= (-> test-vec create-state (move 6) print-vec)
                  [9 4 6 6 5 -8 4 1 -5 -7])
             (is= (-> test-vec create-state (move 7) print-vec)
                  [9 4 4 6 6 5 -8 1 -5 -7])
             (is= (-> test-vec create-state (move 8) print-vec)
                  [9 4 6 -5 6 5 -8 1 4 -7])
             (is= (-> test-vec create-state (move 9) print-vec)
                  [9 4 -7 6 6 5 -8 1 4 -5])))}
  [state orig-idx]
  (let [[curr-idx item] (->> state
                             (mfilter :vals #(= (:orig-index %) orig-idx))
                             (first))
        m (dec (count state))
        steps (:steps-to-move item)]
    (if (zero? steps)
      state
      (let [new-idx (mod (+ curr-idx steps) m)
            new-idx (if (zero? new-idx) m new-idx)
            left-idx (min curr-idx new-idx)
            right-idx (max curr-idx new-idx)
            shift-fn (if (< curr-idx new-idx) dec inc)]
        (-> state
            (dissoc curr-idx)
            (update-keys (fn [key] (if (<= left-idx key right-idx)
                                     (shift-fn key)
                                     key)))
            (assoc new-idx item))))))

(defn mix
  [times state]
  (reduce move state (flatten (repeat times (range (count state))))))

(defn coordinate-sum
  [input decryption-key times]
  (->> input
       (map (partial * decryption-key))
       create-state
       (mix times)
       print-vec
       cycle
       (drop-while #(not= 0 %))
       (take 3001)
       (take-nth 1000)
       (apply +)))

(time (coordinate-sum real-input 1 1))                      ; Part 1 ~10 seconds
(time (coordinate-sum real-input 811589153 10))             ; Part 2 ~90 seconds
