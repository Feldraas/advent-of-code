(ns aoc24.src.day07
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [nmap]]
   [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn ||
  [x y]
  (read-string (str x y)))

(defn unconcat
  [result num]
  (let [result (str result)
        pattern (re-pattern (str num "$"))]
    (read-string (str/replace result pattern ""))))

(defn achievable?
  [[result & numbers]]
  (cond
    (not (int? result)) false
    (= (count numbers) 1)  (= (first numbers) result)
    :else (or
           (achievable? (conj (butlast numbers) (- result (last numbers))))
           (achievable? (conj (butlast numbers) (/ result (last numbers))))
           (and
            ;; false  ;; Uncomment for part 1
            (str/ends-with? (str result) (str (last numbers)))
            (achievable? (conj (butlast numbers) (unconcat result (last numbers))))))))

(->> real-input
     (map (partial re-seq #"\d+"))
     (nmap 2 read-string)
     (filter achievable?)
     (map first)
     (apply +)
     #_(submit-answer 2))

