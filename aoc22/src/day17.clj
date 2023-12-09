(ns aoc22.src.day17
  (:require
    [aoc-tools :refer [read-input]]))

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
        (remove #(= (second %) "@") $)
        (keys $)
        (map second $)
        (if (empty? $)
          0
          (inc (apply max $)))))

(defn rock?
  [cave point]
  (= (get-in cave [:rocks point]) "#"))

(defn free?
  [cave [x y]]
  (cond
    (neg? x) false
    (>= x 7) false
    (neg? y) false
    (rock? cave [x y]) false
    :else true))

(defn valid?
  [cave rock]
  (every? #(free? cave %) rock))

(defn draw-cave
  [cave]
  (as-> (for [y (range (max 15 (+ (get-height cave) 5)))
              x (range 7)]
          (get-in cave [:rocks [x y]] " ")) $
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

(defn get-current-rock
  [cave]
  (->> (:rocks cave)
       (filter #(= (second %) "@"))
       (map first)))

(defn remove-current-rock
  [cave]
  (reduce #(update %1 :rocks dissoc %2) cave (get-current-rock cave)))

(defn update-current-rock
  [cave rock]
  (as-> cave $
        (remove-current-rock $)
        (reduce #(assoc-in %1 [:rocks %2] "@") $ rock)))

(defn spawn-rock
  ([cave]
   (-> cave
       (spawn-rock (first (:queue cave)))
       (update :queue rotate)))
  ([cave rock-type]
   (let [edge [2 (+ 3 (get-height cave))]]
     (update-current-rock cave (create-rock edge rock-type)))))

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
        new-rock (map push-fn (get-current-rock cave))
        new-cave (update cave :jet rotate)]
    (if (valid? new-cave new-rock)
      (update-current-rock new-cave new-rock)
      new-cave)))

(defn stop-current-rock
  [cave]
  (assoc cave :rocks (zipmap (keys (:rocks cave)) (repeat "#"))))

(defn drop-rock
  [cave]
  (let [new-rock (map drop-one-step (get-current-rock cave))]
    (if (valid? cave new-rock)
      (update-current-rock cave new-rock)
      (-> cave
          (stop-current-rock)
          (update :rock-count inc)
          (spawn-rock)))))



(defn floor?
  [cave y]
  (->> cave
       (:rocks)
       (filter #(= (second (first %)) y))
       (filter #(= (second %) "#"))
       (count)
       (= 7)))

(defn find-floor
  [cave]
  (let [height (get-height cave)]
    (as-> (range (- height 100) height) $
          (filter #(floor? cave %) $)
          (if (empty? $)
            0
            (apply max $)))))

(defn truncate-cave
  [cave]
  (let [floor (find-floor cave)]
    (cond
      (zero? floor) cave
      (pos? (mod (:rock-count cave) 10)) cave
      :else (-> cave
                (update :rocks
                        (fn [rocks]
                          (into {} (remove #(<= (second (first %)) floor) rocks))))))))

(defn update-cave
  [cave]
  (-> cave
      (push-rock)
      (drop-rock)
      (truncate-cave)))

(def cave (-> {:jet              (first (read-input))
               :queue            '(:line :cross :corner :column :square)
               :rocks            {}
               :rock-count       0
               :truncated-height 0}))

(time (def final-cave (->> cave
                           (spawn-rock)
                           (iterate update-cave)
                           (take-while #(<= (:rock-count %) 300))
                           (last))))                        ; ~3 mins