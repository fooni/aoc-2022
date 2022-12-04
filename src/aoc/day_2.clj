
(ns aoc.day-2
  (:require [util.input]
            [clojure.string :as s]
            [clojure.set :refer [map-invert]]))
;; Part 1
(def shape->points {:rock 1 :paper 2 :scissors 3})
(def outcome->points {:win 6 :lose 0 :draw 3})
(def beats {:rock :scissors
            :paper :rock
            :scissors :paper})
(def symbol->shape {"A" :rock "B" :paper "C" :scissors
                    "X" :rock "Y" :paper "Z" :scissors})

(defn parse-round [line]
  (map symbol->shape (s/split line #" ")))

(defn outcome [[opponent-shape player-shape]]
  (cond
    (= opponent-shape player-shape) :draw
    (= (beats opponent-shape) player-shape) :lose
    :else :win))

(defn score [outcome player-shape]
  (+
   (outcome->points outcome)
   (shape->points player-shape)))

(defn solution1 [lines]
  (->> lines
       (map parse-round)
       (map (fn [round] (score (outcome round)
                               (second round))))
       (apply +)))

;; Part 2
(def symbol->outcome {"X" :lose "Y" :draw "Z" :win})
(def loses (map-invert beats))

(defn parse-round-expected [line]
  (let [[opponent-sym outcome-sym] (s/split line #" ")]
    (vector (symbol->shape opponent-sym)
            (symbol->outcome outcome-sym))))

(defn select-shape [expected-outcome opponent-shape]
  (condp = expected-outcome
        :win (loses opponent-shape)
        :lose (beats opponent-shape)
        :draw opponent-shape))

(defn solution2 [lines]
  (->> lines
       (map parse-round-expected)
       (map (fn [[opponent-shape expected-outcome]]
                 (score expected-outcome
                        (select-shape expected-outcome
                                      opponent-shape))))
       (apply +)))

(comment
  ;; 1: 9651
  (util.input/with-input-lines "day_2.txt" solution1)
  ;; 2: 10560
  (util.input/with-input-lines "day_2.txt" solution2)

  (def test-input-1 (seq (s/split-lines "A Y
B X
C Z")))
  (= 15 (solution1 test-input-1))
  (= 12 (solution2 test-input-1)))