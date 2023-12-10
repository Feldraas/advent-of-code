(ns aoc23.src.day10
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [get-adjacent in? mfilter iterate-until ->!]]
    [ysera.test :refer [is=]]))

(def input (read-input :test))
(def real-input (read-input))

(defn create-state
  [input]
  (into {} (for [x (range (count input))
                 y (range (count (first input)))
                 :let [ch (nth (nth input x) y)
                       val {:char (if (= ch \.)
                                    \space
                                    ch)}]]
             {[x y] (if (= ch \S)
                      (assoc val :distance 0)
                      val)})))

(defn get-borders
  [state]
  (->> state
       keys
       (filter vector?)
       sort
       last))

(defn draw
  [state & [show-main?]]
  (let [[xmax ymax] (get-borders state)]
    (->> (for [x (range (inc xmax))
               y (range (inc ymax))]
           (as-> [x y] res
                 (get state res)
                 (if (and show-main? (:distance res))
                   \M
                   (:char res))
                 (str res ".")))
         (partition (inc ymax))
         (map (partial apply str)))))


(defn get-pipe-exits
  [state coords]
  (let [curr-pipe (:char (get state coords))
        [down up right left :as exits] (get-adjacent 4 coords)
        [down-pipe up-pipe right-pipe left-pipe] (map #(:char (get state %)) exits)]
    (cond-> []
            (and (in? [\7 \| \F \S] curr-pipe) (in? [\| \L \J] down-pipe)) (conj down)
            (and (in? [\| \L \J \S] curr-pipe) (in? [\| \7 \F] up-pipe)) (conj up)
            (and (in? [\- \L \F \S] curr-pipe) (in? [\- \J \7] right-pipe)) (conj right)
            (and (in? [\- \J \7 \S] curr-pipe) (in? [\- \L \F] left-pipe)) (conj left))))

(defn extend-from
  [state coords]
  (let [{distance :distance} (get state coords)
        exits (get-pipe-exits state coords)
        exits (remove #(:distance (get state %)) exits)]
    (reduce (fn [acc-state exit]
              (assoc-in acc-state [exit :distance] (inc distance)))
            state
            exits)))

(defn single-pass
  [state]
  (let [max-dist (->> state
                      (vals)
                      (filter :distance)
                      (map :distance)
                      (apply max))
        points (->> state
                    (mfilter :vals #(= (:distance %) max-dist))
                    keys)
        next-state (reduce extend-from state points)]
    (if (= state next-state)
      (assoc state :finished true)
      next-state)))

(defn traverse-main-loop
  [input]
  (let [state (create-state input)]
    (iterate-until :finished single-pass state)))

(->> real-input                                             ; ~60 sec
     traverse-main-loop
     vals
     (map :distance)
     (remove nil?)
     (apply max)
     (time)
     #_(submit-answer 1))
