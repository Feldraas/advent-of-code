(ns aoc23.src.day1
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]
    [clojure.string :as str]))

(def calibration (read-input))

(def digit-map (-> (zipmap ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
                           (map str (range 10)))
                   (assoc "zerone" "01" "twone" "21" "oneight" "18" "eightwo" "82")))

(defn text->digit
  [string]
  (reduce #(str/replace %1 %2 (get digit-map %2))
          string
          (sort-by #(- (count %)) (keys digit-map))))

(time (->> calibration
           (map text->digit)                                ; Comment out for part 1
           (map #(str/replace % #"[a-z]+" ""))
           (map (juxt first last))
           (map #(apply str %))
           (map parse-int)
           (apply +)))
