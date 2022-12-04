
(ns aoc.day-3
  (:require [util.input]
            [util.chars :refer [char-range]]
            [clojure.string :as s]
            [clojure.set :as set]))

;; Task 1

;; Item is a lettter from a-z and A-Z (with this priority order)
;; Rucksack is a string of items where the first half of the string 
;; represents items in compartment 1 and the second half items in 
;; compartment 2

(def items (concat
            (char-range \a \z)
            (char-range \A \Z)))

(def item->priority (zipmap items (map inc (range))))

(defn large-compartments [rucksack] 
  (split-at (/ (count rucksack) 2) rucksack))

(defn repeated-items [compartments]
  (apply set/intersection (map set compartments)))

(defn total-priority [items]
  (apply + (map item->priority items)))

(defn solution1 [lines]
  (->> lines
       (map large-compartments)
       (map repeated-items)
       (map total-priority)
       (apply +)))

;; Task 2
(defn all-items [rucksack] 
  (map char rucksack))

(defn common-item [group]
  (->> group
       (map all-items)
       (map set)
       (apply set/intersection)))

(defn solution2 [lines] 
  (->> lines
       (partition 3)
       (map common-item)
       (map total-priority)
       (apply +)))

(comment
  ;; 1: 8185
  (util.input/with-input-lines "day_3.txt" solution1)
  ;; 2: 2817
  (util.input/with-input-lines "day_3.txt" solution2)

  (def test-input-1 (seq (s/split-lines "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"))) 
  (= 157 (solution1 test-input-1))
  
  (def test-input-2 (seq (s/split-lines "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"))) 
  (= 70 (solution2 test-input-2)))
