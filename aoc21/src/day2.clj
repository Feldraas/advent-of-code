(ns aoc21.src.day2
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [parse-int]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(def state1 [0 0])

(defn move-part-1
  [state command]
  (let [[direction steps] (str/split command #" ")
        steps (parse-int steps)]
    (->> direction
         (get {"up"      [-1 0]
               "down"    [1 0]
               "forward" [0 1]})
         (map #(* steps %))
         (map + state))))


(->> real-input
     (reduce move-part-1 state1)
     (apply *)
     time
     #_(submit-answer 1))

(def state2
  {:position [0 0]
   :aim      0})

(defn move-part-2
  [state command]
  (let [[direction steps] (str/split command #" ")
        steps           (parse-int steps)
        position-change (get {"forward" [(* steps (:aim state)) steps]} direction [0 0])
        aim-change      (get {"up" (- steps) "down" steps} direction 0)]
    (-> state
        (update :aim + aim-change)
        (update :position (partial map + position-change)))))

(->> real-input
     (reduce move-part-2 state2)
     :position
     (apply *)
     time
     #_(submit-answer 2))
