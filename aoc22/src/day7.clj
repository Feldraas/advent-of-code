(ns aoc22.src.day7
  (:require [utils :refer [read-input
                           parse-int]]
            [clojure.string :as str]))

(def log (read-input "day7.txt"))

(def state
  {"/"           {}
   :current-path []})

(defn step-into
  [state path]
  (update state :current-path concat [path]))

(defn step-out
  [state]
  (update state :current-path drop-last))

(defn create-dir
  [state dir-name]
  (do (println ""))
  (assoc-in state (concat (:current-path state) [dir-name]) {}))

(defn create-file
  [state command]
  (let [[size-string name] (str/split command #" ")
        size (parse-int size-string)]
    (assoc-in state (concat (:current-path state) [name]) size)))

(defn parse-command
  [state command]
  (cond
    (str/starts-with? command "$ cd ") (let [path (str/replace command "$ cd " "")]
                                         (if (= path "..")
                                           (step-out state)
                                           (step-into state path)))
    (= command "$ ls") state
    (str/starts-with? command "dir") (let [path (str/replace command "dir " "")]
                                       (create-dir state path))
    :else (create-file state command)))

(def tree (get (reduce parse-command state log) "/"))

(defn calc-size
  [obj]
  (cond
    (int? obj) obj
    (every? int? (vals obj)) (apply + (vals obj))
    :else (->> (vals obj) (map calc-size) (apply +))))

(calc-size tree)

(defn reset-after-limit
  [num limit]
  (if (<= num limit)
    num
    0))

(defn calc-size-below-limit
  [obj limit]
  (cond
    (int? obj) 0
    (every? int? (vals obj)) (reset-after-limit (apply + (vals obj)) limit)
    :else (+ (reset-after-limit (calc-size obj) limit)
             (->> (vals obj)
                  (map #(calc-size-below-limit % limit))
                  (apply +)))))

(calc-size-below-limit tree 100000)


(def req-size (- (calc-size tree) 40000000))

(defn find-smallest
  [obj]
  (let [curr-size (cond
                    (int? obj) obj
                    (every? int? (vals obj)) (apply + (vals obj))
                    :else (->> (vals obj) (map find-smallest) (apply +)))]
    (if (> curr-size req-size)
      (do (println curr-size) curr-size)                    ; Hacky -> Check output
      curr-size)))

(find-smallest tree)