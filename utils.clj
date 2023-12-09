(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ysera.test :refer [is=]]))

(defn parse-int [s]
  (if (= s "")
    0
    (Integer/parseInt s)))

(defn create-files
  ([day]
   (let [folder (->> (io/file ".")
                     (.list)
                     (filter #(str/starts-with? % "aoc"))
                     (sort)
                     (last))]
     (create-files day folder)))
  ([day folder]
   (let [input-path (str folder "/inputs/")
         src-path (str folder "/src/")]
     (do
       (spit (str input-path "day" day ".txt") "")
       (spit (str input-path "day" day ".ex") "")
       (spit (str src-path "day" day ".clj") (str "(ns aoc23.src.day" day "\r\n"
                                                  "  (:require \r\n"
                                                  "    [utils :refer [read-input]]))\r\n\r\n"
                                                  "(def input (read-input :test))\r\n"
                                                  "(def real-input (read-input))\r\n\r\n"))))))

(defn read-input
  [& [arg]]
  (let [[year _ day] (-> (str *ns*)
                         (str/split #"\."))
        file (cond
               (nil? arg) (str day ".txt")
               (= arg :test) (str day ".ex")
               :else (str (name arg) ".txt"))
        path (str year "/inputs/" file)]
    (->> (with-open [rdr (io/reader path)]
           (mapv str (line-seq rdr))))))

(defn in?
  "true if coll contains elm"
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

(defn unnil
  [x val]
  (if (nil? x)
    val
    x))

(defn extract-numbers
  [string]
  (->> string
       (re-seq #"-?\d+")
       (map parse-int)))

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
  {:test (fn []
           (is= (mfilter :keys even? {1 :a 2 :b 3 :c 4 :d})
                {2 :b 4 :d})
           (is= (mfilter :vals even? {:a 2 :b 3 :c 4 :d 1})
                {:a 2 :c 4}))}
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
