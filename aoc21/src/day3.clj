(ns aoc21.src.day3
  (:require
    [aoc-tools :refer [read-input submit-answer]]
    [utils :refer [transpose nmap]]))

(def input (read-input :test))
(def real-input (read-input))

(as-> real-input res
      (transpose res)
      (map frequencies res)
      (map #(sort-by second %) res)
      (nmap 2 first res)
      [(map second res) (map first res)]
      (map #(apply str %) res)
      (map #(Long/parseLong % 2) res)
      (apply * res)
      #_(submit-answer 1 res))

(defn position-freqs
  [input]
  (-> (zipmap (range (count (first input))) (map frequencies (transpose input)))
      (update-vals #(sort-by second %))))

(defn rating
  ([input kind]
   (rating input 0 kind))
  ([input idx kind]
   (if (= (count input) 1)
     (first input)
     (let [pos-freqs    (position-freqs input)
           idx-freqs    (get pos-freqs idx)
           equal?       (or
                          (= (second (first idx-freqs)) (second (second idx-freqs))))
           most-common  (first (last idx-freqs))
           oxygen-value (if equal?
                          \1
                          most-common)
           co2-value    (get {\1 \0 \0 \1} oxygen-value)
           check-value  (if (= kind :oxygen) oxygen-value co2-value)]
       (-> (filter #(= (nth % idx) check-value) input)
           (rating (inc idx) kind))))))

(->> [:oxygen :CO2]
     (map #(rating real-input %))
     (map #(Long/parseLong % 2))
     (apply *)
     time
     #_(submit-answer 2))




