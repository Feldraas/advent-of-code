(ns aoc23.src.day12
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [extract-numbers]]
    [clojure.string :as str]
    [ysera.test :refer [is=]]))

(def input (->> (read-input :test)
                (map #(str/split % #" "))
                (map #(vector (first %) (extract-numbers (second %))))))

(def real-input (->> (read-input)
                     (map #(str/split % #" "))
                     (map #(vector (first %) (extract-numbers (second %))))))

(defn all-perms
  [rec]
  (let [num-qs (->> rec
                    (filter #{\?})
                    count)]
    (if (= num-qs 1)
      [(str/replace rec "?" "#") (str/replace rec "?" ".")]
      (->> [(str/replace-first rec "?" "#") (str/replace-first rec "?" ".")]
           (map all-perms)
           (apply concat)))))

(defn parse-groups
  [rec]
  (->> rec
       (re-seq #"#+")
       (map count)))

(defn exhaustive-search
  [[rec groups]]
  (if (= rec "")
    1
    (->> rec
         all-perms
         (map parse-groups)
         (filter #{groups})
         count)))

(defn arrangements
  [input]
  (->> input
       (map #(exhaustive-search [(first %) (second %)]))
       (apply +)))

;(->> real-input
;     arrangements
;     time
;     #_(submit-answer 1))

(defn strip-front
  [[rec groups]]
  (if (or (= (set rec) #{\?})
          (empty? groups))
    [rec groups]
    (case (first rec)
      nil [rec groups]
      \. (strip-front [(str/replace rec #"^\.+" "") groups]) ; Remove starting dots
      \# (strip-front [(apply str (drop (inc (first groups)) rec)) (rest groups)]) ; Drop first group
      \? (let [starting-qs (count (take-while #{\?} rec))
               next        (nth rec starting-qs)]
           (if (and (= next \.) (< starting-qs (first groups))) ; Less ?'s than needed to create first group
             (strip-front [(apply str (drop starting-qs rec)) groups])
             [rec groups])))))

(defn strip-end
  [[rec groups]]
  (let [[rev-rec rev-groups] (strip-front [(str/reverse rec) (reverse groups)])]
    [(str/reverse rev-rec) (reverse rev-groups)]))

(defn strip
  [entry]
  (-> entry strip-front strip-end))

(defn remove-?s-right-of-group
  [[rec groups]]
  (let [max-group    (apply max groups)
        substrings   (re-seq #"\#+\?" rec)
        too-long     (filter #(> (count %) max-group) substrings)
        replacements (map #(str/replace % "?" ".") too-long)]
    (vector (reduce #(str/replace %1 (first %2) (second %2)) rec (map vector too-long replacements))
            groups)))

(defn remove-?s-left-of-group
  [[rec groups]]
  (let [max-group    (apply max groups)
        substrings   (re-seq #"\?\#+" rec)
        too-long     (filter #(> (count %) max-group) substrings)
        replacements (map #(str/replace % "?" ".") too-long)]
    (vector (reduce #(str/replace %1 (first %2) (second %2)) rec (map vector too-long replacements))
            groups)))

(defn remove-?s-next-to-group
  [[rec groups]]
  (-> [rec groups]
      remove-?s-left-of-group
      remove-?s-right-of-group
      strip))

(defn damaged-group-lengths
  [s]
  (let [min-group (take-while #{\#} s)]
    [(count min-group) (count s)]))

(defn possible-lengths-of-first-damaged-groups
  [rec]
  (let [drop-working (str/replace rec #"^\.+" "")
        groups       (str/split drop-working #"\.+")
        lengths      (map damaged-group-lengths groups)
        [first-min first-max] (first lengths)]
    (if (pos? first-min)
      [first-min first-max]
      (let [drop-zero-min (remove #(zero? (first %)) lengths)]
        [(max 1 (apply min 0 (flatten drop-zero-min))) (apply max (flatten lengths))]))))

(defn try-first
  [[rec groups]]
  (if (or (= rec "")
          (nil? groups)
          (empty? groups))
    [rec groups]
    (let [damaged-first      (apply str \# (rest rec))
          lengths-if-damaged (possible-lengths-of-first-damaged-groups damaged-first)
          damaged-possible?  (<= (first lengths-if-damaged) (first groups) (last lengths-if-damaged))
          working-first      (apply str \. (rest rec))
          lengths-if-working (possible-lengths-of-first-damaged-groups working-first)
          working-possible?  (<= (first lengths-if-working) (first groups) (last lengths-if-working))]
      (cond
        (and damaged-possible? working-possible?) [rec groups]
        damaged-possible? (strip [damaged-first groups])
        working-possible? (strip [working-first groups])
        :else :wtf))))

(defn try-last
  [[rec groups]]
  (let [[rev-rec rev-groups] (try-first [(str/reverse rec) (reverse groups)])]
    [(str/reverse rev-rec) (reverse rev-groups)]))

(defn min-size
  [groups]
  (apply + (dec (count groups)) groups))

(defn try-whole
  [[rec groups]]
  (if (= (count (filter #{\? \#} rec)) (min-size groups))
    ["" []]
    [rec groups]))

(defn min-groups
  [rec]
  (as-> rec $
        (str/split $ #"\.+")
        (filter #(str/includes? % "#") $)
        (count $)))

(defn maximize-groups
  [rec]
  (loop [rec rec]
    (let [first-pass (-> rec
                         (str/replace "^??" "#.")
                         (str/replace "??$" ".#")
                         (str/replace ".???." ".#.#.")
                         (str/replace "???" ".#.")
                         (str/replace ".??" ".#.")
                         (str/replace "??." ".#.")
                         (str/replace "#?#" "#.#")
                         (str/replace "??" "#."))]
      (if (= rec first-pass)
        rec
        (recur first-pass)))))

(defn max-groups
  [rec]
  (as-> rec $
        (str/split $ #"\.")
        (map maximize-groups $)
        (map #(str/split % #"\.") $)
        (flatten $)
        (filter #(str/includes? % "#") $)
        (count $)))

(defn invalid?
  [[rec groups]]
  (not (<= (min-groups rec) (count groups) (max-groups rec))))

(defn add-first-group-at-idx
  [[rec groups] idx]
  (let [size         (first groups)
        group-string (apply str (repeat size "#"))
        splt         (str/split rec (re-pattern group-string) 2)
        split?       (> (count splt) 1)
        start        (if split?
                       (str (first splt) group-string)
                       (first splt))
        max-idx      (- (count (first splt)) (if split? 0 size))
        end          (second splt)]
    (if (and (<= idx max-idx) (not (str/includes? (subs rec idx (+ idx size)) ".")))
      (let [candidate (str (apply str (repeat idx \.))
                           group-string
                           (if (< idx max-idx) \.)
                           (subs start (min (count start) (+ idx size 1)) (count start))
                           end)
            bad       (map #(and (= (nth candidate %) \.) (= (nth rec %) \#))
                           (range (count rec)))]
        (if-not (some true? bad)
          candidate)))))

(defn add-last-group-at-idx
  [[rec groups] idx]
  (-> [(str/reverse rec) (reverse groups)]
      (add-first-group-at-idx (- (count rec) idx 1))
      (str)
      (str/reverse)))

(defn edge-group-configs
  [[rec groups] side]
  (let [add-fn (if (= side :first)
                 add-first-group-at-idx
                 add-last-group-at-idx)]
    (->> (range (count rec))
         (map #(add-fn [rec groups] %))
         (remove nil?)
         (remove #(invalid? [% groups])))))

(defn try-group
  [[rec groups] side]
  (if (or (= rec "")
          (nil? groups)
          (empty? groups))
    [rec groups]
    (let [configs (edge-group-configs [rec groups] side)]
      (if (= (count configs) 1)
        (strip [(first configs) groups])
        [rec groups]))))

(defn simplify                                              ; Current progress: #'s eliminated in 482/1000
  [[rec groups :as entry]]
  (let [processed (-> entry strip try-first try-last try-whole
                      (try-group :first) (try-group :last))]
    (if (or (= entry processed)
            (= rec "")
            (empty? groups))
      entry
      (simplify processed))))

(defn q-ways
  [[rec groups]]
  (let [total-qs (count rec)
        qs       (first groups)]
    (cond
      (empty? groups) 1
      (< total-qs (min-size groups)) 0
      (= total-qs qs) 1
      (= groups [1]) total-qs
      :else (->> (range (- total-qs qs -1))
                 (map #(q-ways [(apply str (drop (+ % (first groups) 1) rec)) (rest groups)]))
                 (apply +)))))

(defn q-dot-ways
  {:test (fn []
           (is= (q-dot-ways ["???..?..???.?" [1 1 2]]) 6))}
  [[rec groups]]
  (let [q-counts (->> (re-seq #"\?+" rec)
                      (map count))]
    (cond
      (empty? groups) 1
      (= "" rec) 0
      (< (count rec) (min-size groups)) 0
      (< (apply + q-counts) (apply + groups)) 0
      (= q-counts groups) 1
      ;(every? #(= % 1) groups) (+
      ;                           (->> [(str/replace-first rec "?" ".") groups] simplify q-dot-ways)
      ;                           (->> [(str/replace-first rec #"\?[?.]" "#.") groups] simplify q-dot-ways))
      ;(= (first q-counts) (first groups)) (let [cleaned (str/replace rec #"^\?+\.+" "")]
      ;                                      (if ((set cleaned) \.)
      ;                                        (q-dot-ways [cleaned (rest groups)])
      ;                                        (q-ways [cleaned (rest groups)])))
      ;(= (last q-counts) (last groups)) (let [cleaned (str/replace rec #"\.+\?+$" "")]
      ;                                    (if ((set cleaned) \.)
      ;                                      (q-dot-ways [cleaned (butlast groups)])
      ;                                      (q-ways [cleaned (butlast groups)])))
      :else 0)))

;(defn num-ways
;  {:test (fn []
;           (is= (->> ["???.###" [1 1 3]] simplify num-ways) 1)
;           (is= (->> [".??..??...?##." [1 1 3]] simplify num-ways) 4)
;           (is= (->> ["?#?#?#?#?#?#?#?" [1 3 1 6]] simplify num-ways) 1)
;           (is= (->> ["????.#...#..." [4 1 1]] simplify num-ways) 1)
;           (is= (->> ["????.######..#####." [1 6 5]] simplify num-ways) 4)
;           (is= (->> ["?###????????" [3 2 1]] simplify num-ways) 10)
;           (is= (->> ["???..????..???" [3 2 3]] simplify num-ways) 3))}
;  [[rec groups] & [print?]]
;  (let [_      (if print? (println "Checking" rec groups))
;        result (cond
;                 (= rec "") 1
;                 (= (count groups) 0) 1
;                 (not (str/includes? rec "?")) 1
;                 (= (set rec) #{\?}) (q-ways rec groups)
;                 (= (set rec) #{\? \.}) (q-dot-ways [rec groups])
;                 :else (let [pre-mult (->> (first-group-configs rec groups)
;                                           (map #(str/replace % #"^#*\.*" ""))
;                                           (map #(num-ways [% (rest groups)]))
;                                           (remove zero?))
;                             _        (if print? (println "Premult is" pre-mult))]
;                         (if (empty? pre-mult)
;                           0
;                           (apply * pre-mult))))]
;    result))
