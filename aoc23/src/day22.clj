(ns aoc23.src.day22
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers iterate-until mfilter]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))


(defn create-block
  [string]
  (let [[[x1 y1 z1] [x2 y2 z2]] (map extract-numbers (str/split string #"~"))]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))
          z (range z1 (inc z2))]
      [x y z])))

(defn create-state
  [input]
  (let [all-blocks (map create-block input)
        on-ground  (filter (fn [block] (some #(= % 1) (map last block))) all-blocks)]
    {:falling (->> all-blocks
                   (remove (set on-ground))
                   (sort-by #(apply min (map last %))))
     :stopped (->> on-ground
                   (map-indexed #(zipmap %2 (repeat %1)))
                   (apply merge))}))

(defn drop-block
  [state block]
  (let [dropped      (map #(vector (first %) (second %) (dec (last %))) block)
        ground?      (some #(= % 1) (map last dropped))
        other-block? (some #(get-in state [:stopped %]) dropped)]
    (if (or ground? other-block?)
      (let [block-number (->> state
                              :stopped
                              vals
                              (concat [0])
                              (apply max))]
        (as-> state res
              (update res :falling #(remove #{block} %))
              (reduce (fn [acc-state point]
                        (assoc-in acc-state [:stopped point] (inc block-number)))
                      res
                      (if other-block? block dropped))))
      (-> state
          (update :falling #(remove #{block} %))
          (update :falling conj dropped)))))

(defn drop-lowest
  [state]
  (let [min-z  (->> state
                    :falling
                    (apply concat)
                    (map last)
                    (apply min))
        lowest (->> state
                    :falling
                    (filter #(= (apply min (map last %)) min-z)))]
    (reduce drop-block state lowest)))

(defn get-final-state
  [input]
  (->> input
       create-state
       (iterate-until #(empty? (:falling %)) drop-lowest)
       :stopped))

(defn get-below
  [state number]
  (->> state
       (mfilter :vals #{number})
       keys
       (map #(vector (first %) (second %) (dec (last %))))
       (map state)
       (remove #{number})
       (remove nil?)
       set))

(defn get-above
  [state number]
  (->> state
       (mfilter :vals #{number})
       keys
       (map #(vector (first %) (second %) (inc (last %))))
       (map state)
       (remove #{number})
       (remove nil?)
       set))


(defn disintegratable
  [input]
  (let [final-state (get-final-state input)]
    (->> (range (count input))
         (map #(get-below final-state %))
         (filter #(= (count (set %)) 1))
         flatten
         set
         count
         (- (count input)))))

(->> real-input
     disintegratable
     time
     #_(submit-answer 1))
