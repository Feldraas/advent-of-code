(ns aoc24.src.day23
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [mfilter]]
   [clojure.string :as str]
   [clojure.math.combinatorics :refer [combinations]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-conns
  [input]
  (->> input
       (map #(str/split % #"-"))
       (map set)
       (set)))

(defn get-computers
  [conns]
  (->> conns
       (map vec)
       (flatten)
       (set)))

(defn can-join?
  [computer network conns]
  (->> network
       (map #(vector % computer))
       (map set)
       (every? #(conns %))))

(defn add-computer
  [acc conns computer]
  (let [options (mfilter :vals #(can-join? computer % conns) acc)]
    (reduce #(update %1 %2 conj computer) acc (keys options))))

(defn parse-all-networks
  [input]
  (let [conns (get-conns input)
        computers (get-computers conns)
        start-acc (->> conns
                       (interleave (range (count conns)))
                       (partition 2)
                       (map vec)
                       (into {}))]
    (->> (reduce #(add-computer %1 conns %2) start-acc computers)
         (vals)
         (set))))


;; Part 1
(->> real-input
     (parse-all-networks)
     (map #(combinations % 3))
     (remove nil?)
     (apply concat)
     (set)
     (remove #(not-any?
               (fn [s] (str/starts-with? s "t")) %))
     (count)
     #_(submit-answer 1))

;; Part 2
(->> real-input
     (parse-all-networks)
     (sort-by count)
     (last)
     (sort)
     (str/join ",")
     #_(submit-answer 2))
