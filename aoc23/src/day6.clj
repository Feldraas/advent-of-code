(ns aoc23.src.day6
  (:require
    [utils :refer [read-input parse-int nmap]]))


(def test-races (->> (read-input :test)
                     (map #(re-seq #"\d+" %))
                     (nmap 2 parse-int)
                     (#(zipmap (first %) (second %)))))

(def real-races (->> (read-input)
                     (map #(re-seq #"\d+" %))
                     (nmap 2 parse-int)
                     (#(zipmap (first %) (second %)))))

(defn calc-distance
  [hold-time total-time]
  (* hold-time (- total-time hold-time)))

(defn ways-to-beat
  [[total-time record]]
  (->> (range (inc total-time))
       (map #(calc-distance % total-time))
       (filter #(> % record))
       (count)))


(defn margin
  [races]
  (->> (map ways-to-beat races)
       (apply *)))

(time (margin real-races))

; Part 2

(time (as-> real-races res
            (vector (keys res) (vals res))
            (map #(apply str %) res)
            (map bigint res)
            (vector res)
            (margin res)))

