(ns aoc24.src.day06
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [filter-vals iterate-until]]))

(def input (read-input :test))
(def real-input (read-input))

(def up [-1 0])
(def down [1 0])
(def left [0 -1])
(def right [0 1])

(defn get-state
  [input]
  (let [size (count input)
        char-map {\. #{}
                  \^ #{up}}
        grid (into {} (for [x (range size)
                            y (range size)
                            :let [char (nth (nth input x) y)]]
                        [[x y] char]))

        start-pos (->> grid
                       (filter-vals #{\^})
                       (keys)
                       (first))]
    {:size size
     :obstacles (filter-vals #{\#} grid)
     :position start-pos
     :direction up
     :path {start-pos #{up}}}))

(defn turn-right
  [direction]
  (get {up right
        right down
        down left
        left up} direction))

(defn edge?
  [[x y :as pos] state]
  (or
   (zero? x)
   (zero? y)
   (= x (dec (:size state)))
   (= y (dec (:size state)))))

(defn obstacle?
  [pos state]
  (get-in state [:obstacles pos]))

(defn step
  [state]
  (let [{pos :position
         dir :direction} state
        right (turn-right dir)
        next (map + pos dir)
        dirs-on-next (get-in state [:path next] #{})]
    (cond
      (edge? pos state) (assoc state :stop :edge)
      (dirs-on-next dir) (assoc state :stop :loop)
      (obstacle? next state) (-> state
                                 (update-in [:path pos] conj right)
                                 (assoc :direction right))
      :else (let [state (if
                         (get-in state [:path next])
                          state
                          (assoc-in state [:path next] #{}))]
              (-> state
                  (assoc :position next)
                  (update-in [:path next] conj dir))))))


(defn simulate-walk
  [state]
  (->> (iterate-until :stop step state)))


;; Part 1
(->> real-input
     (get-state)
     (simulate-walk)
     (:path)
     (keys)
     (count)
     #_(submit-answer 1))


(defn add-obstacle-and-simulate-walk
  [state pos]
  (-> state
      (assoc-in [:obstacles pos] \#)
      (simulate-walk)))


(let [input real-input
      state (get-state input)
      size (:size state)
      spots (for [x (range size)
                  y (range size)
                  :let [char (nth (nth input x) y)]
                  :when (= char \.)]
              [x y])]
  (->> spots
       (map #(add-obstacle-and-simulate-walk state %))  ;; Cringe
       (map :stop)
       (filter #{:loop})
       (count)
       #_(submit-answer 2)))
