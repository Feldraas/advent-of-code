(ns aoc22.src.day12
  (:require
    [utils :refer [read-input in? mfilter mremove get-adjacent unnil]]
    [clojure.string :as str]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-start
  [input]
  (->> input
       (map #(str/index-of % "S"))
       (map-indexed vector)
       (mremove :vals nil?)
       (first)))

(defn get-destination
  [input]
  (->> input
       (map #(str/index-of % "E"))
       (map-indexed vector)
       (mremove :vals nil?)
       (first)))

(defn create-state
  [input]
  {:grid        input
   :visited     [(get-destination input)]
   :destination (get-start input)
   :seen        {}})

(defn valid?
  [state [x y]]
  (let [grid (:grid state)
        xmax (count grid)
        ymax (count (first grid))]
    (and
      (not (in? (:visited state) [x y]))
      (>= x 0)
      (>= y 0)
      (< x xmax)
      (< y ymax))))

(defn height-at
  [state [x y]]
  (let [grid-char (nth (nth (:grid state) x) y)]
    (int (get {\S \a \E \z} grid-char grid-char))))

(defn can-reach?
  [state to from]
  (>= (height-at state from) (dec (height-at state to))))

(defn get-exits
  [state point]
  (->> (get-adjacent 4 point)
       (filter (partial valid? state))
       (filter (fn [exit]
                 (can-reach? state point exit)))))

(defn traverse-from
  ([state]
   (traverse-from state (get-start state)))
  ([state from]
   (let [exits (get-exits state from)
         _ (println "Currently visited:" (count (:visited state)))]
     (cond
       (empty? exits) (do
                        ;(println "Cannot exit" from "Current state:" state)
                        ;(println "Created" (assoc-in state [:seen from] ##Inf))
                        (assoc-in state [:seen from] ##Inf))
       (in? (get-exits state from) (:destination state)) (do
                                                           ;(println "Can reach destination directly from" from)
                                                           ;(println "Have now found paths from" (keys (mremove :vals #(= % ##Inf) (:seen state))) "and" from)
                                                           (assoc-in state [:seen from] 1))
       :else (let [
                   ;_ (println "Branching out from" from)
                   removed-exits (update state :visited #(reduce (fn [acc-visited exit]
                                                                   (conj acc-visited exit))
                                                                 %
                                                                 exits))
                   ;_ (println "Reducing traverse looping over" exits)
                   looped-state (loop [curr-state removed-exits
                                       exits-left exits]
                                  (if (empty? exits-left)
                                    curr-state
                                    (recur (traverse-from curr-state (first exits-left)) (rest exits-left))))
                   ;reduced-state (into {} (reduce traverse-from
                   ;                               removed-exits
                   ;                               exits))
                   ;_ (println "Reduce" from "complete")
                   exit-paths (->> (:seen looped-state)
                                   (mfilter :keys #(in? exits %)))
                   ;_ (println "exit paths from" from "are" exit-paths)
                   shortest-path (apply min (unnil (vals exit-paths) [##Inf]))]
               (assoc-in looped-state [:seen from] (inc shortest-path)))))))

(-> (traverse-from (create-state real-input) (get-destination real-input))
    (get-in [:seen (get-destination real-input)]))
