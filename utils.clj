(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (if (= s "")
    0
    (Integer/parseInt s)))

(defn read-input
  [& [test]]
  (let [path (as-> (str *ns*) $
                   (str/split $ #"\.")
                   (str (first $) "/inputs/" (if (nil? test)
                                               (last $)
                                               "test") ".txt"))]
    (->> (with-open [rdr (io/reader path)]
           (mapv str (line-seq rdr))))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


;(->> (with-open [rdr (io/reader "aoc22")]
;       (mapv str (line-seq rdr))))

(defn testfn
  []
  (str *ns*))

(def a "aoc22.src.day1")

(-> a
    (str/split #"\."))
