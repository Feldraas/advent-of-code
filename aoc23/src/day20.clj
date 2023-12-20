(ns aoc23.src.day20
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [mfilter iterate-until ->!]]
    [clojure.string :as str]
    [clojure.math.numeric-tower :refer [lcm]]))

(def input (read-input :test))
(def input2 (read-input :day20.ex2))
(def real-input (read-input))

(defn create-module
  [string]
  (let [[name outputs] (str/split string #" -> ")
        outputs       (str/split outputs #", ")
        stripped-name (apply str (rest name))]
    (cond
      (= name "broadcaster") {"broadcaster" {:name "broadcaster" :type :broadcaster :outputs outputs}}
      (str/starts-with? name "%") {stripped-name {:name stripped-name :type :flip-flop
                                                  :on   false :outputs outputs}}
      (str/starts-with? name "&") {stripped-name {:name   stripped-name :type :conjunction
                                                  :inputs {} :outputs outputs}})))

(defn create-pulse
  [source destination type]
  {:source source :destination destination :type type})

(defn create-state
  [input]
  (let [base-state   {:modules (into {} (map create-module input))
                      :queue   []
                      :low     0
                      :high    0
                      :found?  (zipmap ["pm" "mk" "pk" "hf"] (repeat false))}
        conjunctions (->> base-state
                          :modules
                          (mfilter :vals #(= (:type %) :conjunction))
                          keys
                          set)
        con-inputs   (->> base-state
                          :modules
                          (map #(interleave (repeat (first %)) (filter conjunctions (:outputs (second %)))))
                          (remove empty?)
                          flatten
                          (partition 2))]
    (reduce (fn [acc-state [inp con]]
              (update-in acc-state [:modules con] #(update % :inputs assoc inp :low)))
            base-state
            con-inputs)))

(defn send-pulses
  [state pulses]
  (let [pulse-type (->> pulses
                        (map :type)
                        first)]
    (-> state
        (update :queue rest)
        (update :queue concat pulses)
        (update pulse-type + (count pulses)))))

(defn process-pulse
  [state]
  (let [{source :source destination :destination type :type} (first (:queue state))
        {receiver-type :type outputs :outputs :as receiver} (get-in state [:modules destination])]
    (case receiver-type
      nil (-> state
              (update :queue rest))
      :broadcaster (let [response-pulses (map #(create-pulse "broadcaster" % type) outputs)]
                     (send-pulses state response-pulses))
      :flip-flop (if (= type :high)
                   (update state :queue rest)
                   (let [on?             (get receiver :on)
                         response-type   (if on? :low :high)
                         response-pulses (map #(create-pulse destination % response-type) outputs)]
                     (-> state
                         (send-pulses response-pulses)
                         (update-in [:modules destination :on] not))))
      :conjunction (let [memory          (get receiver :inputs)
                         memory          (assoc memory source type)
                         all-high?       (= (set (vals memory)) #{:high})
                         response-type   (if all-high? :low :high)
                         response-pulses (map #(create-pulse destination % response-type) outputs)]
                     (-> (if (and (not all-high?) ((set (keys (:found? state))) destination))
                           (assoc-in state [:found? destination] true)
                           state)
                         (send-pulses response-pulses)
                         (assoc-in [:modules destination :inputs] memory))))))

(defn button-press
  [state]
  (iterate-until
    #(empty? (:queue %))
    process-pulse
    (-> state
        (update :queue conj (create-pulse "button" "broadcaster" :low))
        (update :low inc))))

(->> real-input
     create-state
     (iterate button-press)
     (->! nth 1000)
     ((juxt :low :high))
     (apply *)
     time
     #_(submit-answer 1))

(defn cycle-length
  [state con]
  (->> state
       (iterate button-press)
       (take-while #(not (get-in % [:found? con])))
       count))

(let [state   (create-state real-input)
      feeders (keys (:found? state))]
  (->> feeders
       (map #(cycle-length state %))
       (reduce lcm)
       time
       #_(submit-answer 2)))
