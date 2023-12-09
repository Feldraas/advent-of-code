(ns aoc22.src.day5
  (:require
    [aoc-tools :refer [read-input]]
    [utils :refer [parse-int]]))

(def commands (read-input))

(def crates {1 (map char "MSJLVFNR")
             2 (map char "HWJFZDNP")
             3 (map char "GDCRW")
             4 (map char "SBN")
             5 (map char "NFBCPWZM")
             6 (map char "WMRP")
             7 (map char "WSLGNTR")
             8 (map char "VBNFHTQ")
             9 (map char "FNZHML")})

(defn move-one
  [crates from-col to-col]
  (let [crate (first (get crates from-col))]
    (-> crates
        (update from-col rest)
        (update to-col conj crate))))

(defn parse-command
  [crates command]
  (let [[_ num from-col to-col] (re-find (re-matcher #"move (\d+) from (\d+) to (\d+)" command))]
    (nth (iterate #(move-one % (parse-int from-col) (parse-int to-col)) crates) (parse-int num))))

(->> (reduce parse-command crates commands)
     (sort)
     (map second)
     (map first)
     (apply str))

(defn move-multiple
  [crates n from-col to-col]
  (let [stack (take n (get crates from-col))]
    (-> crates
        (update from-col #(drop n %))
        (update to-col #(concat stack %)))))

(defn improved-parse-command
  [crates command]
  (let [[_ num from-col to-col] (re-find (re-matcher #"move (\d+) from (\d+) to (\d+)" command))]
    (move-multiple crates (parse-int num) (parse-int from-col) (parse-int to-col))))

(->> (reduce improved-parse-command crates commands)
     (sort)
     (map second)
     (map first)
     (apply str))