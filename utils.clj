(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (if (= s "")
    0
    (Integer/parseInt s)))

(defn read-input
  [& [file]]
  (let [path (as-> (str *ns*) $
                   (str/split $ #"\.")
                   (str (first $) "/inputs/" (if (nil? file)
                                               (last $)
                                               (name file)) ".txt"))]
    (->> (with-open [rdr (io/reader path)]
           (mapv str (line-seq rdr))))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn get-adjacent
  [[x y] n]
  (case n
    4 (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])
    5 (conj (get-adjacent [x y] 4) [x y])
    8 (concat (get-adjacent [x y] 4) [[(inc x) (inc y)] [(inc x) (dec y)] [(dec x) (inc y)] [(dec x) (dec y)]])
    9 (conj (get-adjacent [x y] 8) [x y])))

(defn non-neg?
  [x]
  (>= x 0))

(defn non-pos?
  [x]
  (<= x 0))