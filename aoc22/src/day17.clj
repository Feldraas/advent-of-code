(ns aoc22.src.day17
  (:require [utils :refer [read-input
                           in?]]
            [clojure.string :as str]))

(defn rotate
  [object]
  (let [reversed (flatten (list (rest object) (first object)))]
    (if (string? object)
      (apply str reversed)
      reversed)))

(defn get-height
  [cave]
  (as-> cave $
        (:rocks $)
        (map second $)
        (if (empty? $)
          0
          (inc (apply max $)))))


(defn rock?
  [cave point]
  (in? (:rocks cave) point))

(defn current-rock?
  [cave point]
  (in? (:current-rock cave) point))

(defn free?
  [cave [x y]]
  (cond
    (neg? x) false
    (>= x 7) false
    (neg? y) false
    (in? (:rocks cave) [x y]) false
    :else true))

(defn valid?
  [cave rock]
  (every? #(free? cave %) rock))

(defn draw-cave
  [cave]
  (as-> (for [y (range (max 15 (+ (get-height cave) 5)))
              x (range 7)]
          (cond
            (rock? cave [x y]) "#"
            (current-rock? cave [x y]) "@"
            :else ".")) $
        (partition 7 $)
        (reverse $)
        (map #(apply str %) $)
        (map #(str "|" % "|") $)
        (concat $ '("+-------+"))))

(defn create-rock
  [[x y] rock-type]
  (case rock-type
    :line [[x y] [(+ x 1) y] [(+ x 2) y] [(+ x 3) y]]
    :cross [[(+ x 1) (+ y 2)] [(+ x 1) (+ y 1)] [(+ x 1) y] [x (+ y 1)] [(+ x 2) (+ y 1)]]
    :corner [[(+ x 2) (+ y 2)] [(+ x 2) (+ y 1)] [(+ x 2) y] [(+ x 1) y] [x y]]
    :column [[x y] [x (+ y 1)] [x (+ y 2)] [x (+ y 3)]]
    :square [[x y] [(+ x 1) y] [x (+ y 1)] [(+ x 1) (+ y 1)]]))

(defn spawn-rock
  ([cave]
   (-> cave
       (spawn-rock (first (:queue cave)))
       (update :queue rotate)))
  ([cave rock-type]
   (let [edge [2 (+ 3 (get-height cave))]]
     (assoc cave :current-rock (create-rock edge rock-type)))))

(defn push
  [[x y] direction]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]))

(defn drop-one-step
  [[x y]]
  [x (dec y)])

(defn push-rock
  [cave]
  (let [push-fn (if (= (first (:jet cave)) \>)
                  #(push % :right)
                  #(push % :left))
        new-rock (map push-fn (:current-rock cave))
        new-cave (update cave :jet rotate)]
    (if (valid? new-cave new-rock)
      (assoc new-cave :current-rock new-rock)
      new-cave)))

(defn drop-rock
  [cave]
  (let [new-rock (map drop-one-step (:current-rock cave))]
    (if (valid? cave new-rock)
      (assoc cave :current-rock new-rock)
      (-> cave
          (update :rocks concat (:current-rock cave))
          (update :rock-count inc)
          (spawn-rock)))))

(defn update-cave
  [cave]
  (-> cave
      (push-rock)
      (drop-rock)))

(def cave (-> {:jet          (first (read-input :test))
               :queue        '(:line :cross :corner :column :square)
               :current-rock []
               :rocks        []
               :rock-count   0}
              (spawn-rock)))

;(take-while #(< (:rock-count %) 2021) (iterate update-cave cave))
(-> (nth (iterate update-cave cave) 1000)
    (draw-cave))