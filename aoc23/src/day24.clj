(ns aoc23.src.day24
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [nmap]]))

(def input (read-input :test))
(def real-input (read-input))

(defn get-lines
  [input]
  (->> input
       (map #(re-seq #"-?\d+" %))
       (nmap 2 bigint)
       (map #(partition 3 %))))

(defn intersection
  [[[px1 py1 pz1 :as p1] [vx1 vy1 vz1 :as v1]] [[px2 py2 pz2] [vx2 vy2 vz2]]]
  (let [disc (- (* vx2 vy1) (* vx1 vy2))]
    (if-not (zero? disc)
      (let [dpx (- px2 px1)
            dpy (- py2 py1)
            t1  (/ (+ (* (- vy2) dpx) (* vx2 dpy)) disc)
            t2  (/ (+ (* (- vy1) dpx) (* vx1 dpy)) disc)]
        (if (and (pos? t1) (pos? t2))
          (->> v1
               (map (partial * t1))
               (map + p1)
               (map float)))))))

(defn find-intersections
  [input low high]
  (let [lines (get-lines input)]
    (->> (for [l1 lines
               l2 lines
               :when (not= l1 l2)]
           (intersection l1 l2))
         (map butlast)
         set
         (remove nil?)
         (filter #(<= low (first %) high))
         (filter #(<= low (second %) high)))))

(->> (find-intersections real-input 200000000000000 400000000000000)
     count
     time
     #_(submit-answer 1))

