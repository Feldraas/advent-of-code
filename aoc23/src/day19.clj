(ns aoc23.src.day19
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers parse-int iterate-until]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn parse-part
  [string]
  (zipmap (seq "xmas") (extract-numbers string)))

(defn parse-workflow
  [string]
  (let [[name flow] (str/split string #"\{")
        rules (str/split (subs flow 0 (dec (count flow))) #",")]
    {name rules}))

(defn create-state
  [input]
  (let [[rules _ parts] (partition-by #{""} input)]
    {:workflows (apply merge (map parse-workflow rules))
     :parts     (map parse-part parts)
     :accepted  (list)
     :rejected  (list)}))

(defn check-rule
  [rule part]
  (if-not (str/includes? rule ":")
    rule
    (let [ctg (first rule)
          op  (resolve (symbol (str (second rule))))
          [_ num next-flow] (first (re-seq #"(\d+):(\w+)" rule))]
      (if (op (get part ctg) (parse-int num))
        next-flow
        nil))))

(defn check-workflow
  [workflow part]
  (let [all-results (map #(check-rule % part) workflow)]
    (first (remove nil? all-results))))

(defn process-part
  ([state]
   (process-part state "in"))
  ([state workflow]
   (let [part   (first (:parts state))
         result (check-workflow (get-in state [:workflows workflow]) part)]
     (case result
       "A" (-> state
               (update :parts rest)
               (update :accepted conj part))
       "R" (-> state
               (update :parts rest)
               (update :rejected conj part))
       (process-part state result)))))

(defn total-rating
  [input]
  (->> input
       create-state
       (iterate-until #(empty? (:parts %)) process-part)
       :accepted
       (map vals)
       flatten
       (apply +)))

(->> real-input
     total-rating
     time
     #_(submit-answer 1))
