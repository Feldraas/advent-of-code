(ns aoc22.src.day22
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [mfilter mremove parse-int in? ->! iterate-until]]))

(def input (read-input :test))
(def real-input (read-input))

(defn create-grid
  [input]
  (into {} (for [x (range (- (count input) 2))
                 y (range (apply max (map count input)))
                 :let [c (nth (nth input x) y \space)]]
             {[x y] (case c
                      \. :open
                      \# :wall
                      \space :wrap)})))

(defn parse-path
  [path]
  (->> path
       (partition-by #(in? (map char (range 48 58)) %))
       (map (partial apply str))
       (map #(if (#{"R" "L"} %) % (parse-int %)))))

(defn create-state
  [input]
  (let [grid (create-grid input)]
    {:grid      grid
     :path      (parse-path (last input))
     :position  (->> grid
                     (mfilter :vals #{:open})
                     keys
                     sort
                     first)
     :direction [0 1]}))

(defn turn
  [[x y] how]
  (case how
    "R" [y (- x)]
    "L" [(- y) x]))

(defn next-position
  [state position direction]
  (let [new-position (map + position direction)
        new-tile (get-in state [:grid new-position] :wrap)]
    (case new-tile
      :open new-position
      :wall position
      :wrap (let [wrap (if (pos? (apply + direction))
                         first
                         last)
                  wrap-position (->> state
                                     :grid
                                     (mremove :vals #{:wrap})
                                     keys
                                     (filter (fn [[x y]] (if (zero? (first direction))
                                                           (= x (first position))
                                                           (= y (second position)))))

                                     sort
                                     wrap)
                  wrap-tile (get-in state [:grid wrap-position])]
              (if (= wrap-tile :open)
                wrap-position
                position)))))



(defn move
  [{position  :position
    path      :path
    direction :direction :as state}]
  (cond
    (string? (first path)) (-> state
                               (update :direction turn (first path))
                               (update :path (comp vec rest)))
    (zero? (first path)) (update state :path (comp vec rest))
    :else (let [new-position (next-position state position direction)]
            (if (= position new-position)
              (update state :path (comp vec rest))
              (-> state
                  (assoc :position new-position)
                  (update-in [:path 0] dec))))))

(defn final-state
  [state]
  (iterate-until #(empty? (:path %)) move state))

(defn password
  [input]
  (let [fstate (->> input
                    create-state
                    final-state)]
    (+
      (* 1000 (inc (first (:position fstate))))
      (* 4 (inc (second (:position fstate))))
      (get {[0 1]  0
            [1 0]  1
            [0 -1] 2
            [-1 0] 3} (:direction fstate)))))


(->> (password real-input)
     (time)
     #_(submit-answer 1))
