(ns aoc23.src.day12
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers]]
    [clojure.string :as str]))

(def input (->> (read-input :test)
                (map #(str/split % #" "))
                (map #(vector (first %) (extract-numbers (second %))))))

(def real-input (->> (read-input)
                     (map #(str/split % #" "))
                     (map #(vector (first %) (extract-numbers (second %))))))

(defn unfold
  [[rec groups]]
  [(str/join "?" (repeat 5 rec)) (apply concat (repeat 5 groups))])

(defn ways
  [[rec groups :as entry] cache]
  (cond
    (get cache entry) [(get cache entry) cache]
    (= entry ["" []]) [1 (assoc cache entry 1)]
    (= rec "") [0 (assoc cache entry 0)]
    (empty? groups) (if (str/includes? rec "#")
                      [0 (assoc cache entry 0)]
                      [1 (assoc cache entry 1)])
    (= (first rec) \.) (ways [(str/replace rec #"^\.+" "") groups] cache)
    (= (first rec) \?) (let [[dot-ways dot-cache] (ways [(apply str (rest rec)) groups] cache)
                             [hash-ways hash-cache] (ways [(apply str \# (rest rec)) groups] dot-cache)]
                         [(+ dot-ways hash-ways) (assoc hash-cache entry (+ dot-ways hash-ways))])
    (or (< (count rec) (first groups))
        (str/includes? (apply str (take (first groups) rec)) ".")
        (str/starts-with? (subs rec (first groups) (count rec)) "#")) [0 cache]
    :else (ways [(apply str (drop (inc (first groups)) rec)) (rest groups)] cache)))

(defn total-arrangements
  [input]
  (->> (reduce (fn [[acc-vec acc-cache] entry]
                 (let [[num-ways new-cache] (ways entry acc-cache)]
                   [(conj acc-vec num-ways) new-cache]))
               [[] {}]
               input)
       first
       (apply +)))

(->> real-input
     total-arrangements
     time
     #_(submit-answer 1))

(->> real-input
     (map unfold)
     total-arrangements
     time
     #_(submit-answer 2))
