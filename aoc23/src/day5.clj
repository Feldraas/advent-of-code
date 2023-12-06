(ns aoc23.src.day5
  (:require [utils :refer [read-input nmap in?]]))


(defn get-almanac
  [& [arg]]
  (->> (read-input arg)
       (partition-by #(= % ""))
       (take-nth 2)))

(def test-almanac (get-almanac :test))
(def real-almanac (get-almanac))

(defn get-seeds
  [almanac]
  (->> (first almanac)
       (first)
       (re-seq #"\d+")
       (map bigint)))

(defn get-seed-ranges
  [almanac]
  (->> (get-seeds almanac)
       (partition 2)
       (map #(vector (first %) (+ (first %) (second %) -1)))))


(defn get-almanac-map
  [almanac n]
  (->> (nth almanac n)
       (rest)
       (map #(re-seq #"\d+" %))
       (nmap 2 bigint)))

(defn find-next
  [almanac-map num step]
  (do
    (if (empty? almanac-map)
      num
      (let [range (first almanac-map)
            range-start (second range)
            range-end (+ (second range) (last range) -1)
            diff (- (first range) (second range))]
        (if (<= range-start num range-end)
          (+ num diff)
          (find-next (rest almanac-map) num step))))))

(defn find-seed-location
  ([almanac seed]
   (find-seed-location almanac seed (dec (count almanac))))
  ([almanac seed n]
   (reduce #(find-next (get-almanac-map almanac %2) %1 %2) seed (range 1 (inc n)))))

(defn trace
  [almanac seed]
  (map #(find-seed-location almanac seed %) (range (count almanac))))

(time (let [seeds (get-seeds real-almanac)]
        (->> (map #(find-seed-location real-almanac %) seeds)
             (apply min))))

; Part 2

(defn join-into-limit-map
  [left right]
  (let [[[left-low left-high] left-diff] left]
    (->> (for [[[right-low right-high] right-diff] right]
           [[(max left-low (- right-low left-diff)) (min left-high (- right-high left-diff))] (+ left-diff right-diff)])
         (remove #(> (first (first %)) (second (first %)))))))

(defn join-limits
  [left right]
  (->> left
       (map #(join-into-limit-map % right))
       (apply concat)))

(defn get-max-value
  [almanac]
  (let [max-seed (->> (get-seed-ranges almanac)
                      (map second)
                      (apply max))
        max-inc (->> (map #(get-almanac-map almanac %) (range 1 8))
                     (nmap 2 #(- (first %) (second %)))
                     (map #(apply max %))
                     (apply +))]
    (+ max-seed max-inc)))

(defn fill-map
  [lmap max-value]
  (let [ordered (sort-by first lmap)
        starts (map #(first (first %)) ordered)
        ends (map #(second (first %)) ordered)
        missing-starts (->> ends
                            (map inc)
                            (remove #(in? starts %))
                            (remove #(= (inc (apply max ends)) %))
                            (sort))
        missing-ends (->> starts
                          (map dec)
                          (remove #(in? ends %))
                          (remove #(= -1 %))
                          (sort))

        reduced (reduce #(assoc %1 [(nth missing-starts %2) (nth missing-ends %2)] 0)
                        lmap
                        (range (count missing-ends)))
        reduced-starts (map first (keys reduced))
        reduced-ends (map second (keys reduced))]
    (cond-> reduced
            (not (in? reduced-starts 0)) (assoc [0 (dec (apply min reduced-starts))] 0)
            (not (in? reduced-ends max-value)) (assoc [(inc (apply max reduced-ends)) max-value] 0))))

(defn get-limit-map
  [almanac n]
  (as-> (get-almanac-map almanac n) $
        (map #(vector [(second %) (+ (second %) (last %) -1)] (- (first %) (second %))) $)
        (into {} $)
        (fill-map $ (get-max-value almanac))))

(defn get-final-map
  [almanac]
  (->> (map #(get-limit-map almanac %) (range 1 8))
       (reduce join-limits)
       (sort-by first)))

(defn get-min-location-for-range
  [almanac seed-range]
  (let [locs (->> (get-final-map almanac)
                  (filter #(and
                             (>= (second (first %)) (first seed-range))
                             (<= (first (first %)) (second seed-range))))
                  (sort-by first)
                  (map #(+ (first (first %)) (last %))))]
    (apply min locs)))

(time (as-> (get-seed-ranges real-almanac) $
            (map #(get-min-location-for-range real-almanac %) $)
            (apply min $)))
