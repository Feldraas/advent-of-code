(ns aoc22.src.day24
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [get-adjacent in?]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-borders
  [input]
  [(count input) (count (first input))])

(defn create-state
  [input]
  (let [[xmax ymax] (get-borders input)]
    (-> (apply merge-with conj
               {\> [] \< [] \v [] \^ []}
               (for [x (range (count input))
                     y (range (apply max (map count input)))
                     :let [ch (nth (nth input x) y)]
                     :when (#{\> \< \^ \v} ch)]
                 {ch [x y]}))
        (update-keys {\> [0 1]
                      \< [0 -1]
                      \v [1 0]
                      \^ [-1 0]})
        (assoc :borders [xmax ymax]))))

(defn move-blizzard
  [borders blizzard-coords blizzard-direction]
  (let [[xmax ymax] borders
        [new-x new-y] (map + blizzard-coords blizzard-direction)]
    [(get {(dec xmax) 1 0 (- xmax 2)} new-x new-x)
     (get {(dec ymax) 1 0 (- ymax 2)} new-y new-y)]))

(defn move
  [state]
  (let [borders (:borders state)]
    (-> state
        (update [0 1] (fn [blizzards] (map #(move-blizzard borders % [0 1]) blizzards)))
        (update [0 -1] (fn [blizzards] (map #(move-blizzard borders % [0 -1]) blizzards)))
        (update [1 0] (fn [blizzards] (map #(move-blizzard borders % [1 0]) blizzards)))
        (update [-1 0] (fn [blizzards] (map #(move-blizzard borders % [-1 0]) blizzards))))))

(defn get-exits-from
  [[x y] borders]
  (let [[xmax ymax] borders]
    (cond
      (= [x y] [0 1]) [[0 1] [1 1]]
      (= [x y] [1 1]) [[1 1] [0 1] [2 1] [1 2]]
      :else (conj (->> (get-adjacent 4 [x y])
                       (remove #(<= (first %) 0))
                       (remove #(and
                                  (>= (first %) (dec xmax))
                                  (not= % [(- xmax 1) (- ymax 2)])))
                       (remove #(<= (second %) 0))
                       (remove #(>= (second %) ymax)))
                  [x y]))))

(defn free?
  [state point]
  (as-> state res
        (dissoc res :borders)
        (vals res)
        (apply concat res)
        (not-any? #{point} res)))

(defn minutes-to-reach
  ([input]
   (let [borders (get-borders input)
         destination [(- (first borders) 1) (- (second borders) 2)]]
     (minutes-to-reach input (create-state input) [0 1] destination)))
  ([input starting-state start-position end-position]
   (let [borders (get-borders input)]
     (loop [steps 0
            current-state starting-state
            current-positions [start-position]]
       (if (in? current-positions end-position)
         steps
         (let [next-state (move current-state)
               next-positions (->> current-positions
                                   (map #(get-exits-from % borders))
                                   (apply concat)
                                   set
                                   (filter #(free? next-state %)))]
           (recur (inc steps) next-state next-positions)))))))

(defn there-and-back-and-there-again
  [input]
  (let [start [0 1]
        borders (get-borders input)
        destination [(- (first borders) 1) (- (second borders) 2)]
        state0 (create-state input)
        minutes-there (minutes-to-reach input state0 start destination)
        state-there (nth (iterate move state0) minutes-there)
        minutes-back (minutes-to-reach input state-there destination start)
        state-back (nth (iterate move state-there) minutes-back)
        minutes-there-again (minutes-to-reach input state-back start destination)]
    (+ minutes-there minutes-back minutes-there-again)))

(->> real-input
     minutes-to-reach
     time
     #_(submit-answer 1))
(->> real-input
     there-and-back-and-there-again
     time
     #_(submit-answer 2))
