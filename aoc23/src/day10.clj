(ns aoc23.src.day10
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [get-adjacent in? mfilter iterate-until ->!]]
    [clojure.string :as str]
    [ysera.test :refer [is=]]))

(def input (read-input :test))
(def input2 (read-input :day10.ex2))
(def input3 (read-input :day10.ex3))
(def input4 (read-input :day10.ex4))
(def input5 (read-input :day10.ex5))
(def real-input (read-input))

(def LR \─)
(def UD \│)
(def UL \┐)
(def UR \┌)
(def DL \┘)
(def DR \└)

(defn between-horizontal-pipes?
  [state [x y]]
  (let [left (get state [x (dec y)])
        right (get state [x (inc y)])]
    (and (in? [LR DR UR \S] (:char left))
         (in? [LR DL UL \S] (:char right)))))

(defn between-vertical-pipes?
  [state [x y]]
  (let [up (get state [(dec x) y])
        down (get state [(inc x) y])]
    (and (in? [UD UR UL \S] (:char up))
         (in? [UD DR DL \S] (:char down)))))

(defn connect-pipes
  [state]
  (->> state
       (map (fn [[point data]]
              [point (cond
                       (between-horizontal-pipes? state point) (assoc data :char LR :type :pipe)
                       (between-vertical-pipes? state point) (assoc data :char UD :type :pipe)
                       :else data)]))
       (into {})))

(defn create-state
  [input]
  (->> (for [x (range (dec (* 2 (count input))))
             y (range (dec (* 2 (count (first input)))))]
         (if (or (odd? x) (odd? y))
           {[x y] {:type :ground :source :extra :char \space}}
           (let [orig-char (nth (nth input (/ x 2)) (/ y 2))]
             {[x y] (case orig-char
                      \S {:type :pipe :source :orig :char orig-char :distance 0}
                      \. {:type :ground :source :orig :char \space}
                      {:type :pipe :source :orig :char (get {\- LR \| UD \7 UL \J DL \F UR \L DR} orig-char)})})))
       (into {})
       (connect-pipes)))

(defn get-borders
  [state]
  (->> state
       keys
       sort
       last))

(defn draw
  [state & [compress?]]
  (let [state (if (map? state) state (create-state state))
        state (if compress?
                (-> (mfilter (partial every? even?) state)
                    (update-keys #(vec (map (comp int (partial * 0.5)) %))))
                state)
        [xmax ymax] (get-borders state)]
    (->> (for [x (range (inc xmax))
               y (range (inc ymax))]
           (as-> [x y] res
                 (get state res)
                 (:char res)))
         (partition (inc ymax))
         (map (partial apply str)))))

(defn get-pipe-exits
  [state [x y :as coords]]
  (let [curr-pipe (:char (get state coords))
        down [(+ x 2) y]
        up [(- x 2) y]
        right [x (+ y 2)]
        left [x (- y 2)]
        [down-pipe up-pipe right-pipe left-pipe] (map #(:char (get state %)) [down up right left])]
    (cond-> []
            (and (in? [UL UD UR \S] curr-pipe) (in? [UD DR DL] down-pipe)) (conj down)
            (and (in? [UD DR DL \S] curr-pipe) (in? [UD UL UR] up-pipe)) (conj up)
            (and (in? [LR DR UR \S] curr-pipe) (in? [LR DL UL] right-pipe)) (conj right)
            (and (in? [LR DL UL \S] curr-pipe) (in? [LR DR UR] left-pipe)) (conj left))))

(defn extend-main-pipe
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
        next-state (reduce extend-main-pipe state points)]
    (if (= state next-state)
      (assoc state :finished true)
      next-state)))

(defn traverse-main-pipe
  [input]
  (let [state (create-state input)
        final-state (iterate-until :finished single-pass state)]
    (dissoc final-state :finished)))

(defn furthest-point
  [input]
  (->> input
       traverse-main-pipe
       vals
       (map :distance)
       (remove nil?)
       (apply max)))

(->> real-input
     (furthest-point)                                       ; Slow (~4 min) - why?
     (time)
     #_(submit-answer 1))

(defn get-ground-exits
  [state coords]
  (->> (get-adjacent 4 coords)
       (filter #(= (:type (get state %)) :ground))))

(defn expand-pocket
  [state start-points]
  (loop [state (reduce #(assoc-in %1 [%2 :char] \X) state start-points)
         start-points start-points]
    (let [exits (->> start-points
                     (map #(get-ground-exits state %))
                     (apply concat)
                     (remove #(in? [\X \S] (:char (get state %))))
                     (set)
                     (vec))]
      (if (empty? exits)
        state
        (recur (reduce (fn [acc-state exit]
                         (assoc-in acc-state [exit :char] \X))
                       state
                       exits)
               exits)))))

(defn replace-all-chars
  [state orig new]
  (update-vals state (fn [m] (update-vals m #(get {orig new} % %)))))

(defn expand-once
  [state]
  (let [start (->> state
                   (mfilter :vals #(= (:type %) :ground))
                   (mfilter :vals #(not (in? [\O \I \S] (:char %))))
                   (sort-by first)
                   first
                   first)
        expanded-state (expand-pocket state start)
        [xmax ymax] (get-borders state)
        hit-border? (->> expanded-state
                         (mfilter (fn [[x y]]
                                    (or (in? [0 xmax] x)
                                        (in? [0 ymax] y))))
                         vals
                         (map :char)
                         (filter #{\X})
                         (not-empty))]
    (replace-all-chars expanded-state \X (if hit-border? \O \I))))

(defn all-expanded?
  [state]
  (->>
    state
    vals
    (map :char)
    (filter #{\space})
    empty?))

(defn junk-pipe?
  [val]
  (not (or (:distance val) (= (:source val) :extra) (not= (:type val) :pipe))))

(defn remove-junk-pipes
  [state]
  (update-vals state #(if (junk-pipe? %)
                        {:type :ground :source :orig :char \space}
                        %)))

(defn final-state
  [input]
  (->> input
       traverse-main-pipe
       remove-junk-pipes
       (iterate-until all-expanded? expand-once)))

(defn enclosed-tiles
  [input]
  (->> input
       final-state
       (mfilter :vals #(= (:source %) :orig))
       (mfilter :vals #(= (:char %) \I))
       count))

(->> real-input
     (enclosed-tiles)                                       ; Slow (~4 min) - why?
     (time)
     #_(submit-answer 2))