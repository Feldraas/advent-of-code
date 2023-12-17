(ns aoc23.src.day16
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [iterate-until ->! indices mfilter mremove]]
    [clojure.set :as s]))

(def input (read-input :test))
(def real-input (read-input))

(def up [-1 0])
(def down [1 0])
(def left [0 -1])
(def right [0 1])

(defn create-state
  [input starting-tile starting-direction]
  (as-> (for [x (indices input)
              y (indices (first input))
              :let [ch (nth (nth input x) y)]]
          {[x y] {:mirror    ch
                  :energized #{}
                  :expanded  #{}}}) res
        (into {} res)
        (update-in res [starting-tile :energized] conj starting-direction)))

(defn valid?
  [state [x y]]
  (let [xmax (apply max (map first (keys state)))
        ymax (apply max (map second (keys state)))]
    (and (<= 0 x xmax) (<= 0 y ymax))))

(defn expand-in-direction
  [state tile [dx dy :as direction]]
  (let [{mirror :mirror} (get state tile)
        state     (update-in state [tile :expanded] conj direction)
        new-tiles (case mirror
                    \. (list [(map + tile direction) direction])
                    \| (if (zero? dy)
                         (list [(map + tile direction) direction])
                         (list [(map + tile up) up] [(map + tile down) down]))
                    \- (if (zero? dx)
                         (list [(map + tile direction) direction])
                         (list [(map + tile left) left] [(map + tile right) right]))
                    \\ (let [new-direction (get {left  up
                                                 up    left
                                                 right down
                                                 down  right} direction)]
                         (list [(map + tile new-direction) new-direction]))
                    \/ (let [new-direction (get {left  down
                                                 down  left
                                                 right up
                                                 up    right} direction)]
                         (list [(map + tile new-direction) new-direction])))]
    (reduce #(update-in %1 [(first %2) :energized] conj (second %2))
            state
            (filter #(valid? state (first %)) new-tiles))))

(defn expand
  [state tile]
  (let [{energized-directions :energized expanded-directions :expanded} (get state tile)]
    (reduce #(expand-in-direction %1 tile %2) state (s/difference energized-directions expanded-directions))))

(defn expand-all
  [state]
  (->> (reduce expand state (->> state
                                 (mremove :vals #(= (:energized %) (:expanded %)))
                                 keys))))

(defn final?
  [state]
  (->> state
       vals
       (remove #(= (:energized %) (:expanded %)))
       empty?))

(defn count-energized
  [state]
  (->> state
       vals
       (map :energized)
       (remove empty?)
       (count)))

(defn final-energized-count
  ([input]
   (final-energized-count input [0 0] right))
  ([input starting-tile starting-direction]
   (->> (create-state input starting-tile starting-direction)
        (iterate-until final? expand-all)
        count-energized)))

(->> input
     final-energized-count
     time
     #_(submit-answer 1))

(defn get-entrances
  [input]
  (let [xmax      (dec (count input))
        ymax      (dec (count (first input)))
        entrances (for [x (range (inc xmax))
                        y (range (inc ymax))
                        :when (or (zero? x)
                                  (zero? y)
                                  (= x xmax)
                                  (= y ymax))]
                    [[x y] (cond
                             (zero? x) down
                             (zero? y) right
                             (= x xmax) up
                             (= y ymax) left)])]
    (-> entrances
        (conj [[0 0] right])
        (conj [[0 ymax] left])
        (conj [[xmax 0] up])
        (conj [[xmax ymax] left]))))

(defn exit?
  [[[x y] {mirror :mirror energized :energized}]]
  (case mirror
    \. (or
         (and (= x 0) (energized up))
         (and (= y 0) (energized left))
         (and (= x 109) (energized down))
         (and (= y 109) (energized right)))
    \| (and
         (or (= x 0) (= x 109)) (not-empty energized))
    \- (and
         (or (= y 0) (= y 109)) (not-empty energized))
    \\ (or
         (and (= x 0) (energized left))
         (and (= x 109) (energized right))
         (and (= y 0) (energized up))
         (and (= y 109) (energized down)))
    \/ (or
         (and (= x 0) (energized right))
         (and (= x 109) (energized left))
         (and (= y 0) (energized down))
         (and (= y 109) (energized up)))))

(defn max-energized-count
  [input entrance cache]
  (if (get cache (first entrance))
    cache
    (let [final-state (->> entrance
                           (apply create-state input)
                           (iterate-until final? expand-all))
          final-count (count-energized final-state)
          exits       (->> final-state
                           (filter exit?)
                           (map first))]
      (merge-with (partial max)
                  cache
                  {(first entrance) final-count}
                  (zipmap exits (repeat final-count))))))

(->> real-input
     get-entrances
     (reduce (fn [acc-cache entrance] (max-energized-count real-input entrance acc-cache)) {})
     vals
     (apply max)
     time                                                   ; Hours
     #_(submit-answer 2))