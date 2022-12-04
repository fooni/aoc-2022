
(ns aoc.day-1
  (:require util.input))

;; Task 1
(defn max-calories [lines] (->> lines
                                (partition-by empty?)
                                (remove (comp empty? first))
                                (map #(map util.input/parse-int %))
                                (map (partial apply +))
                                vec))

(defn solution1 [lines]
  (apply max (max-calories lines)))

;; Task 2
(defn solution2 [lines]
  (->> lines
       (max-calories)
       (sort)
       (reverse)
       (take 3)
       (apply +)))

(comment
  ;; 1: 71023
  (util.input/with-input-lines "day_1.txt" solution1)
  ;; 2: 206289
  (util.input/with-input-lines "day_1.txt" solution2))

  

  