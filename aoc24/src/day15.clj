(ns aoc24.src.day15
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [mfilter]]
   [clojure.string :as str]))

(def input (read-input :day15.ex2))
(def real-input (read-input))

(def up [-1 0])
(def down [1 0])
(def left [0 -1])
(def right [0 1])

(defn widen
  [s]
  (-> s
      (str/replace "#" "##")
      (str/replace "O" "[]")
      (str/replace "." "..")
      (str/replace "@" "@.")))

(defn parse
  [input & wide?]
  (let [[grid _ moves] (->> input
                            (partition-by #{""}))
        grid (if wide?
               (map widen grid)
               grid)
        grid (into {} (for [x (range (count grid))
                            y (range (count (first grid)))
                            :let [char (nth (nth grid x) y)]
                            :when (not= char \.)]
                        [[x y] char]))
        start-pos (->> grid
                       (mfilter :vals #{\@})
                       (keys)
                       (first))
        grid (-> grid
                 (dissoc start-pos)
                 (assoc :position start-pos))
        moves (map {\< left \> right \^ up \v down} (apply str moves))]

    [grid moves]))

(defn box?
  [char]
  (#{\O \[ \]} char))

(defn wall?
  [char]
  (= char \#))

(defn push-into
  [grid pos direction]
  (let [next (->> (map + pos direction)
                  (iterate (partial map + direction))
                  (partition-by #(wall? (grid %)))
                  (first)
                  (filter #(nil? (grid %)))
                  (first))]
    (if (nil? next)
      grid
      (-> grid
          (dissoc pos)
          (assoc :position pos)
          (assoc next \O)))))

(defn draw
  [grid]
  (let [[x-max y-max] (as-> grid res
                        (keys res)
                        (remove keyword? res)
                        [(map first res) (map second res)]
                        [(apply max (first res)) (apply max (second res))])
        printout (->> (for [x (range (inc x-max))
                            y (range (inc y-max))]
                        (if (= [x y] (grid :position))
                          \@
                          (get grid [x y] \.)))
                      (partition (inc y-max))
                      (map (partial apply str))
                      (str/join "\n"))]
    (do (println printout) grid)))

(defn move
  [grid direction]
  (let [next (map + (grid :position) direction)]
    (cond
      (wall? (grid next)) grid
      (box? (grid next)) (-> grid
                             (push-into next direction))
      :else (assoc grid :position next))))

(defn gps
  [[x y]]
  (+ (* 100 x) y))


;; Part 1
(let [[grid moves] (parse real-input)]
  (->> moves
       (reduce move grid)
       (mfilter :vals #{\O})
       (keys)
       (map gps)
       (apply +)
       #_(submit-answer 1)))




(defn push-wide
  [grid pos direction]
  (cond
    (nil? (grid pos)) grid
    (wall? (grid pos)) grid
    :else (let [[left right] (if (= (grid pos) \[)
                               [pos (map + pos right)]
                               [(map + pos left) pos])
                next-left (map + left direction)
                next-right (map + right direction)
                required (remove #{left right} [next-left next-right])
                grid (reduce #(push-wide %1 %2 direction) grid required)]
            (if (every? nil? (map grid required))
              (->  grid
                   (dissoc left right)
                   (assoc next-left \[ next-right \]))
              grid))))

(defn move-wide
  [grid direction]
  (let [next (map + (grid :position) direction)]
    (cond
      (wall? (grid next)) grid
      (nil? (grid next)) (assoc grid :position next)
      :else (let [pushed-grid (push-wide grid next direction)]
              (if (nil? (pushed-grid next))
                (assoc pushed-grid :position next)
                pushed-grid)))))

;; Part 2
(let [[grid moves] (parse real-input :wide)]
  (->> moves
       (reduce move-wide grid)
       (draw)
       (mfilter :vals #{\[})
       (keys)
       (map gps)
       (apply +)
       #_(submit-answer 2)))
