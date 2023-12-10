(ns aoc22.src.day21
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [parse-int in? intstring? rotate ->! iterate-until]]
    [clojure.string :as str]
    [ysera.test :refer [is=]]))

(def input (read-input :test))
(def real-input (read-input))

(defn create-state
  [input]
  {:lines input})

(defn parse-next-line
  [state]
  (let [line (first (:lines state))
        state (update state :lines rotate)
        [monkey job] (str/split line #": ")]
    (if (intstring? job)
      (assoc state monkey (parse-int job))
      (let [[monkey1 op-char monkey2] (str/split job #" ")
            num1 (get state monkey1)
            num2 (get state monkey2)
            op (resolve (symbol op-char))]
        (if (and num1 num2)
          (assoc state monkey (op num1 num2))
          state)))))

(defn get-root-number
  [input]
  (->> input
       create-state
       (iterate parse-next-line)
       (take-while #(nil? (get % "root")))
       last
       parse-next-line
       (->! get "root")))

;(->> (time (get-root-number real-input))
;     #_(submit-answer 1))

; Part 2

(defn format-job
  [job]
  (let [[left op right] (str/split job #" ")]
    (if (intstring? job)
      job
      (str "(" op " " left " " right ")"))))

(defn create-state-2
  [input]
  (->> input
       (map #(str/split % #": "))
       (into {})
       (->! update-vals format-job)
       (->! dissoc "humn")))

(defn insert-monkey
  [state monkey]
  (-> state
      (dissoc monkey)
      (update-vals (fn [job] (str/replace job monkey (get state monkey))))))

(defn eval-monkey
  [state monkey]
  (update state monkey (fn [job]
                         (try
                           (str (eval (read-string job)))
                           (catch Exception e job)))))

(defn process-monkey
  [state monkey]
  (if (intstring? (get state monkey))
    (insert-monkey state monkey)
    (eval-monkey state monkey)))

(defn single-pass
  [state]
  (reduce process-monkey state (keys state)))

(defn parse-expr
  [expr]
  (-> expr
      (subs 1 (dec (count expr)))
      (str/split #" ")))

(defn can-insert?
  [state]
  (some intstring? (vals state)))

(defn can-eval?
  [state]
  (->> (vals state)
       (map #(re-seq #"\d \d" %))
       (some some?)))

(defn get-halfway-state
  [state]
  (iterate-until (complement #(or (can-insert? %) (can-eval? %))) single-pass state))

(defn process-expression
  {:test (fn []
           (is= (process-expression "(- abc 15)" 30) ["abc" 45])
           (is= (process-expression "(- 15 abc)" 30) ["abc" -15])
           (is= (process-expression "(+ abc 15)" 30) ["abc" 15])
           (is= (process-expression "(+ 15 abc)" 30) ["abc" 15])
           (is= (process-expression "(* abc 15)" 30) ["abc" 2])
           (is= (process-expression "(* 15 abc)" 30) ["abc" 2])
           (is= (process-expression "(/ abc 15)" 30) ["abc" 450])
           (is= (process-expression "(/ 15 abc)" 30) ["abc" 1/2]))}
  [expr val]
  (let [[op left right] (parse-expr expr)
        op-fn (resolve (symbol op))
        var (if (intstring? left) right left)
        num (if (intstring? left) left right)
        num-first (= num left)
        num (bigint num)
        inverse (get {"+" "-" "-" "+" "*" "/" "/" "*"} op)
        inverse-fn (resolve (symbol inverse))]
    [var (if (and num-first
                  (in? ["/" "-"] op))
           (op-fn num val)
           (inverse-fn val num))]))


(defn propagate-backwards
  [halfway]
  (let [[value monkey] (->> (get halfway "root")
                            (parse-expr)
                            (rest)
                            (sort))]
    (loop [expr (get halfway monkey)
           value (bigint value)]
      (let [[new-monkey new-value] (process-expression expr value)
            new-expr (get halfway new-monkey)]
        (if (str/includes? expr "humn")
          new-value
          (recur new-expr new-value))))))

(defn get-human-number
  [input]
  (->> real-input
       (create-state-2)
       (get-halfway-state)
       (propagate-backwards)))


(->> (time (get-human-number real-input))
     #_(submit-answer 2))