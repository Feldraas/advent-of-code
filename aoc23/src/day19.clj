(ns aoc23.src.day19
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers parse-int iterate-until ->!]]
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

(defn merge*
  [m1 m2]
  (-> (merge (dissoc m1 "A") (dissoc m2 "A"))
      (assoc "A" (concat (get m1 "A") (get m2 "A")))))

(defn split-by-rules
  [intervals rules]
  (if (= (count rules) 1)
    {(first rules) intervals}
    (let [rule              (first rules)
          ctg               (first rule)
          op                (second rule)
          [_ n next-flow] (first (re-seq #"(\d+):(\w+)" rule))
          n                 (parse-int n)
          matches           (update intervals ctg (if (= op \>)
                                                    #(vector (inc n) (second %))
                                                    #(vector (first %) (dec n))))
          non-matches       (update intervals ctg (if (= op \>)
                                                    #(vector (first %) n)
                                                    #(vector n (second %))))
          recursive-matches (split-by-rules non-matches (rest rules))]

      (merge* {next-flow matches} recursive-matches))))

(defn get-all-workflows
  [input]
  (->> (partition-by #{""} input)
       first
       (map parse-workflow)
       (apply merge)))

(defn process-one
  [state all-workflows]
  (let [workflow  (->> state
                       keys
                       (remove #{"A" "R"})
                       first)
        rules     (get all-workflows workflow)
        intervals (get state workflow)]
    (merge* (dissoc state workflow) (split-by-rules intervals rules))))


(defn total-combinations
  [input]
  (let [all-workflows (get-all-workflows input)]
    (->> {"in" (zipmap (seq "xmas") (repeat [1 4000])) "A" []}
         (iterate-until #(= (set (keys %)) #{"A" "R"})
                        #(process-one % all-workflows))
         (->! get "A")
         (map second)
         (map #(- (second %) (first %) -1))
         (partition 4)
         (map #(apply * %))
         (apply +))))


(->> real-input
     total-combinations
     time
     #_(submit-answer 2))