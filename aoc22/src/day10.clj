(ns aoc22.src.day10
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]
    [clojure.string :as str]))

(def commands (read-input))

(def state [{:completed-cycles 0
             :register         1}])

(defn parse-command
  [state command]
  (if (= command "noop")
    (conj state (-> (last state)
                    (update :completed-cycles inc)))
    (conj state
          (update (last state) :completed-cycles inc)
          (-> (last state)
              (update :completed-cycles + 2)
              (update :register + (parse-int (last (str/split command #" "))))))))

(def history (reduce parse-command state commands))

(defn pixel-drawn
  [state]
  (let [cycle (inc (:completed-cycles state))
        draw-position (mod (dec cycle) 40)
        center (:register state)
        diff (abs (- draw-position center))]
    (if (<= diff 1)
      "#"
      ".")))

(->> history
     (partition 40)
     (map #(map pixel-drawn %))
     (map #(apply str %)))