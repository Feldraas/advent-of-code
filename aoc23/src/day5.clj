(ns aoc23.src.day5
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [nmap ->!]]))


(defn get-almanac
  [& [arg]]
  (->> (read-input arg)
       (partition-by #{""})
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
       (map (fn [[start length]]
              (vector start (+ start length -1))))))

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
            shift (- (first range) (second range))]
        (if (<= range-start num range-end)
          (+ num shift)
          (find-next (rest almanac-map) num step))))))

(defn find-seed-location
  ([almanac seed]
   (find-seed-location almanac seed (dec (count almanac))))
  ([almanac seed n]
   (reduce (fn [acc-location current-map-number]
             (->> current-map-number
                  (get-almanac-map almanac)
                  (->! find-next acc-location current-map-number)))
           seed
           (range 1 (inc n)))))

(time (let [all-seeds (get-seeds real-almanac)]
        (->> all-seeds
             (map (fn [seed]
                    (find-seed-location real-almanac seed)))
             (apply min))))

; Part 2

(defn fill-map
  [limit-map]
  (let [starts (map (fn [[[start end] shift]] start) limit-map)
        ends (map (fn [[[start end] shift]] end) limit-map)
        required-starts (-> (map inc ends)
                            (conj 0))
        required-ends (-> (map dec starts)
                          (conj ##Inf))
        missing-starts (remove (set starts) required-starts)
        missing-ends (remove (set ends) required-ends)]
    (reduce (fn [acc-map [missing-start missing-end]]
              (assoc acc-map [missing-start missing-end] 0))
            limit-map
            (map vector missing-starts missing-ends))))

(defn get-limit-map
  [almanac n]
  (->> (get-almanac-map almanac n)
       (map (fn [[dest-start source-start length]]
              (vector [source-start (+ source-start length -1)] (- dest-start source-start))))
       (into {})
       (fill-map)))

(defn absorb-new-limit
  [limit-map new-limit]
  (let [[[llow lhigh] lshift] new-limit]
    (->> limit-map
         (map (fn [[[rlow rhigh] rshift]]
                [[(max llow (- rlow lshift)) (min lhigh (- rhigh lshift))] (+ lshift rshift)]))
         (remove (fn [[[low high] diff]]
                   (> low high))))))

(defn splice-into-next-map
  [current-map next-map]
  (->> current-map
       (map (partial absorb-new-limit next-map))
       (apply concat)))

(defn get-final-map
  [almanac]
  (->> (map (partial get-limit-map almanac)
            (range 1 (count almanac)))
       (reduce splice-into-next-map)
       (sort-by first)))

(defn get-min-location-for-range
  [almanac seed-range]
  (let [locs (->> (get-final-map almanac)
                  (filter (fn [[[start end] shift]]
                            (and
                              (>= end (first seed-range))
                              (<= start (second seed-range)))))
                  (sort-by first)
                  (map (fn [[[start end] shift]]
                         (+ start shift))))]
    (apply min locs)))

(time (as-> (get-seed-ranges real-almanac) $
            (map #(get-min-location-for-range real-almanac %) $)
            (apply min $)))