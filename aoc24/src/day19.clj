(ns aoc24.src.day19
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [mfilter]]
   [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))


(defn get-patterns
  [input]
  (->> (str/split (first input) #", ")
       (interleave (repeat 0))
       (partition 2)
       (map reverse)
       (map vec)
       (into {})))

(defn get-designs
  [input]
  (drop 2 input))

(def patterns (get-patterns real-input))
(def designs (get-designs real-input))

(defn possible?
  [design patterns]
  (prn "Checking" design)
  (cond (nil? design) (do (prn "OK") true)
        (patterns design) (do (prn "OK") true)
        :else (let [candidates (->> patterns
                                    (mfilter #(str/starts-with? design %)))
                    _ (prn "Candidatseese for" design "are" candidates)]
                (if (empty? candidates)
                  (do (prn "Not OK") false)
                  (some true? (map #(possible? (str/replace-first design % "") patterns) (sort-by count > (keys candidates))))))))

;; (let [input real-input
;;       designs (get-designs input)
;;       patterns (get-patterns input)]
;;   (->> input
;;        (get-designs)
;;        (take 10)
;;        (map #(possible? % (get-patterns input)))
;;        (filter true?)
;;        (count)
;;        #_(submit-answer 1)))
