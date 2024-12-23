(ns aoc24.src.day12
  (:require
   [aoc-tools :refer [read-input submit-answer]]
   [utils :refer [get-adjacent mfilter mremove iterate-until ->! in?]]))

(def input (read-input "day12.ex3"))
(def real-input (read-input))

(defn get-plants
  [input]
  (into {} (for [x (range (count input))
                 y (range (count (first input)))]
             [[x y] {:plant (nth (nth input x) y)
                     :region nil}])))

(defn map-region-for
  ([plants pos]
   (->> plants
        (vals)
        (map :region)
        (remove nil?)
        (apply max 0)
        (inc)
        (map-region-for plants pos)))
  ([plants pos n]
   (let [same (->> pos
                   (get-adjacent 4)
                   (filter #(=
                             (get-in plants [pos :plant])
                             (get-in plants [% :plant])))
                   (remove #(get-in plants [% :region])))
         plants (assoc-in plants [pos :region] n)]
     (if (empty? same)
       plants
       (reduce #(map-region-for %1 %2 n) plants same)))))

(defn map-next
  [plants]
  (let [pos (->> plants
                 (mremove :vals :region)
                 (keys)
                 (sort)
                 (first))]
    (if (nil? pos)
      plants
      (map-region-for plants pos))))

(defn map-all
  [plants]
  (->> plants
       (iterate map-next)
       (drop-while #(->> %
                         (vals)
                         (map :region)
                         (filter nil?)
                         (seq)))
       (first)))

(defn get-fence
  [region]
  (->> region
       (keys)
       (map #(get-adjacent 4 %))
       (apply concat)
       (remove (set (keys region)))
       (frequencies)
       (into {:sides 0})))

(defn perimeter-price
  [region]
  (* (count region) (->> region
                         (get-fence)
                         (vals)
                         (apply +))))

(def down [1 0])
(def right [0 1])

(defn follow
  [fence start direction]
  (let [next (map + start direction)]
    (if (zero? (get fence next 0))
      #{start}
      (conj (follow fence next direction) start))))

(defn count-sides
  [fence]
  (let [start (->> fence
                   (mremove :vals zero?)
                   (mremove :keys keyword?)
                   (keys)
                   (sort)
                   (first))
        hrz (follow fence start right)
        vrt (follow fence start down)

        longest (->> [hrz vrt]
                     (sort-by count)
                     (last))]
    (if start
      (as-> fence res
        (update res :sides inc)
        (reduce (fn [fence pos] (update fence pos dec)) res longest)
        (count-sides res))
      fence)))


(defn side-price
  [region]
  (* (count region) (:sides (count-sides (get-fence region)))))

;; Part 1 and 2 (?)
(time
 (->> real-input
      (get-plants)
      (map-all)
      (sort-by (comp :region second))
      (partition-by (comp :region second))
      (map (juxt perimeter-price side-price))
      (apply map +)))
