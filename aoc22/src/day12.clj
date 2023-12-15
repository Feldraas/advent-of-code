(ns aoc22.src.day12
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [iterate-until mfilter get-adjacent in?]]))

(def input (read-input :test))
(def real-input (read-input))

(defn create-state
  [input]
  (into {} (for [x (range (count input))
                 y (range (count (first input)))
                 :let [height-char (nth (nth input x) y)
                       height      (int (get {\S \a \E \z} height-char height-char))]]
             {[x y] (merge {:char height-char :height height}
                           (if (= height-char \E)
                             {:path 0}))})))

(defn can-enter?
  [state from-coords to-coords]
  (let [from-height (:height (get state from-coords))
        to-height   (:height (get state to-coords))]
    (>= from-height (dec to-height))))

(defn extend-from
  [state coords]
  (let [{distance :path} (get state coords)
        entrances (->> (get-adjacent 4 coords)
                       (filter #(get state %))
                       (filter #(can-enter? state % coords))
                       (remove #(:path (get state %))))]
    (reduce (fn [acc-state entrance]
              (assoc-in acc-state [entrance :path] (inc distance)))
            state
            entrances)))

(defn single-pass
  [state]
  (let [max-dist   (->> state
                        (vals)
                        (filter :path)
                        (map :path)
                        (apply max))
        points     (->> state
                        (mfilter :vals #(= (:path %) max-dist))
                        keys)
        next-state (reduce extend-from state points)]
    (if (= state next-state)
      (assoc state :finished true)
      next-state)))

(defn traverse-map
  [input]
  (let [state (create-state input)]
    (iterate-until :finished single-pass state)))

(defn shortest-path-from
  [input chars]
  (->> (traverse-map input)
       (mfilter :vals #(in? chars (:char %)))
       (mfilter :vals :path)
       (vals)
       (map :path)
       (apply min)))

(->> (shortest-path-from real-input [\S])
     (time)
     #_(submit-answer 1))

(->> (shortest-path-from real-input [\S \a])
     (time)
     #_(submit-answer 2))
