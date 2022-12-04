(ns aoc.day-4
  (:require util.input
            [clojure.string :as s]))

;; Task 1
(defn parse-range [id-range]
  (map util.input/parse-int (s/split id-range #"-")))

(defn parse-pair [line]
  (map parse-range (s/split line #",")))

(defn contains [range-a range-b]
  (<= (first range-a) 
      (first range-b) 
      (second range-b) 
      (second range-a)))

(defn assignements-fully-overlap [[a b]]
  (or (contains a b)
      (contains b a)))

(defn solution1 [lines]
  (->> lines
       (map parse-pair)
       (filter assignements-fully-overlap)
       (count)))

;; Task 2
(defn overlap [range-a range-b]
  (<= (first range-a)
      (first range-b) 
      (second range-a)))

(defn assignements-overlap [[a b]]
  (or (overlap a b)
      (overlap b a)))
(defn solution2 [lines] 
  (->> lines
       (map parse-pair)
       (filter assignements-overlap)
       (count)))

(comment
   ;; 1: 444
  (util.input/with-input-lines "day_4.txt" solution1)
  ;; 2: 801
  (util.input/with-input-lines "day_4.txt" solution2)
  
  
  (def test-input-1 (seq (s/split-lines "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"))) 
  (= 2 (solution1 test-input-1))
  (= 4 (solution2 test-input-1))
  
  )
  
