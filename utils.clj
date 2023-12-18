(ns utils)

(defn parse-int [s]
  (cond
    (char? s) (Integer/parseInt (str s))
    (= s "") 0
    :else (Integer/parseInt s)))

(defn extract-numbers
  [string]
  (->> string
       (re-seq #"-?\d+")
       (map parse-int)))

(defn intstring?
  [s]
  (re-seq #"^-?\d+$" (str s)))

(defn in?
  [coll elm]
  (some #(= elm %) coll))

(defn get-adjacent
  [n [x y]]
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

(defn nmap
  ([f coll]
   (nmap 2 f coll))
  ([n f coll]
   (if (= n 1)
     (map f coll)
     (map #(nmap (dec n) f %) coll))))

(defn rotate
  [[head & tail]]
  (concat tail (list head)))

(defn ->!
  [f & args]
  (apply f (last args) (butlast args)))

(defn ->>!
  [arg f & args]
  (apply f (concat args [arg])))

(defn filter-keys
  [f m]
  (->> m
       (filter (fn [[key value]] (f key)))
       (into {})))

(defn filter-vals
  [f m]
  (->> m
       (filter (fn [[key value]] (f value)))
       (into {})))

(defn mfilter
  ([f m]
   (mfilter :keys f m))
  ([by f m]
   (case by
     :keys (filter-keys f m)
     :vals (filter-vals f m))))

(defn mremove
  ([f m]
   (mremove :keys f m))
  ([by f m]
   (mfilter by (complement f) m)))

(defn iterate-until
  [condition f start-val]
  (->> start-val
       (iterate f)
       (take-while (complement condition))
       (last)
       (f)))

(defn indices
  ([coll]
   (indices coll 0 0))
  ([coll drop-start drop-end]
   (->> (range (count coll))
        (drop drop-start)
        (drop-last drop-end))))

(defn second-last
  [coll]
  (last (butlast coll)))

(defn transpose
  [list-of-seqs]
  (let [transposed (->> list-of-seqs
                        (apply mapv vector))]
    (if (some string? list-of-seqs)
      (map #(apply str %) transposed)
      transposed)))

(defn remove-first
  [item-or-pred coll]
  (let [pred (if (ifn? item-or-pred)
               item-or-pred
               #{item-or-pred})
        [before after] (split-with (complement pred) coll)]
    (concat before (rest after))))

