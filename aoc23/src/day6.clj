(ns aoc23.src.day6
  (:require [utils :refer [read-input parse-int nmap]]
            [ysera.test :refer [is is-not is=]]
            [clojure.string :as str]
            [clojure.set :as cset]))


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

(margin real-races)

; Part 2

(->> real-races
     (#(vector (keys %) (vals %)))
     (map #(apply str %))
     (map bigint)
     (vector)
     (margin))

