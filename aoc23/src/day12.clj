(ns aoc23.src.day12
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers]]
    [clojure.string :as str]
    [ysera.test :refer [is=]]))

(def input (->> (read-input :test)
                (map #(str/split % #" "))
                (map #(vector (first %) (extract-numbers (second %))))))

(def real-input (->> (read-input)
                     (map #(str/split % #" "))
                     (map #(vector (first %) (extract-numbers (second %))))))
(defn all-perms
  [rec]
  (let [num-qs (->> rec
                    (filter #{\?})
                    count)]
    (if (= num-qs 1)
      [(str/replace rec "?" "#") (str/replace rec "?" ".")]
      (->> [(str/replace-first rec "?" "#") (str/replace-first rec "?" ".")]
           (map all-perms)
           (apply concat)))))

(defn parse-groups
  [rec]
  (->> rec
       (re-seq #"#+")
       (map count)))

(defn exhaustive-search
  [[rec groups]]
  (if (= rec "")
    1
    (->> rec
         all-perms
         (map parse-groups)
         (filter #{groups})
         count)))

(defn arrangements
  [input]
  (->> input
       (map #(exhaustive-search [(first %) (second %)]))
       (apply +)))

(->> real-input
     arrangements
     time
     #_(submit-answer 1))
