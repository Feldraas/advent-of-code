(ns aoc22.src.day9
  (:require [utils :refer [read-input
                           parse-int]]))

(def commands (read-input "day9.txt"))

(defn create-state
  [num-knots]
  {:knots   (zipmap (range num-knots) (repeat num-knots {:x 0 :y 0}))
   :visited [[0 0]]})

(defn move-head
  [state direction]
  (cond
    (= direction "R") (update-in state [:knots 0 :x] inc)
    (= direction "L") (update-in state [:knots 0 :x] dec)
    (= direction "U") (update-in state [:knots 0 :y] inc)
    (= direction "D") (update-in state [:knots 0 :y] dec)
    :else state))

(defn step
  [diff]
  (if (zero? diff)
    0
    (/ diff (abs diff))))

(defn update-tail
  ([state tail-num]
   (let [hx (get-in state [:knots (dec tail-num) :x])
         hy (get-in state [:knots (dec tail-num) :y])
         tx (get-in state [:knots tail-num :x])
         ty (get-in state [:knots tail-num :y])
         x-diff (- hx tx)
         y-diff (- hy ty)
         adjacent? (<= (apply max (map abs [x-diff y-diff])) 1)]
     (if adjacent?
       state
       (as-> state $
             (update-in $ [:knots tail-num :x] #(+ % (step x-diff)))
             (update-in $ [:knots tail-num :y] #(+ % (step y-diff)))
             (update $ :visited conj (vals (get-in $ [:knots (dec (count (:knots state)))])))))))
  ([state]
   (reduce update-tail state (range 1 (count (:knots state))))))

(defn move
  [state direction]
  (-> state
      (move-head direction)
      (update-tail)))

(defn parse-command
  [state command]
  (let [[_ direction num] (re-find (re-matcher #"^(.) (.+)$" command))]
    (nth (iterate #(move % direction) state) (parse-int num))))

(let [rope-length 10]
  (as-> (create-state rope-length) $
        (reduce parse-command $ commands)
        (:visited $)
        (set $)
        (count $)))