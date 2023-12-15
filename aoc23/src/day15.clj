(ns aoc23.src.day15
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [parse-int in? ->! indices]]
    [clojure.string :as str]))

(def input (str/split (first (read-input :test)) #","))
(def real-input (str/split (first (read-input)) #","))

(defn hash-char
  [starting-value ch]
  (-> starting-value
      (+ (int ch))
      (* 17)
      (mod 256)))

(defn hash-string
  [s]
  (reduce hash-char 0 s))

(defn hash-sum
  [input]
  (->> input
       (map hash-string)
       (apply +)))

(->> (hash-sum real-input)
     time
     #_(submit-answer 1))

(def state (zipmap (range 256) (repeat [])))

(defn remove-lens
  [state lens box-number]
  (update state box-number (fn [lens-list] (vec (remove #(= (first %) lens) lens-list)))))

(defn add-lens
  [state lens box-number focal-length]
  (update state box-number conj [lens focal-length]))

(defn update-lens
  [state lens box-number focal-length]
  (update state box-number (fn [lens-list] (->> lens-list
                                                (map #(if (= (first %) lens)
                                                        [lens focal-length]
                                                        %))
                                                vec))))

(defn follow-step
  [state step]
  (let [[lens focal-length] (str/split step #"[=-]")
        box-number    (hash-string lens)
        lenses-in-box (->> (get state box-number)
                           (map first))]
    (cond
      (nil? focal-length) (remove-lens state lens box-number)
      (in? lenses-in-box lens) (update-lens state lens box-number (parse-int focal-length))
      :else (add-lens state lens box-number (parse-int focal-length)))))

(defn box-focusing-power
  [focal-lengths]
  (->> focal-lengths
       indices
       (map inc)
       (map * focal-lengths)
       (apply +)))


(defn total-focusing-power
  [input]
  (->> input
       (reduce follow-step state)
       (->! update-keys inc)
       (->! update-vals #(map second %))
       (->! update-vals box-focusing-power)
       (map #(* (first %) (second %)))
       (apply +)))

(->> real-input
     total-focusing-power
     time
     #_(submit-answer 2))