
(ns aoc.day-6
  (:require util.input))

(def packet-marker-len 4)
(def message-marker-len 14)

(defn marker? [coll] 
  (= (count coll) 
     (count (set coll))))

(defn marker-index [marker-len signal]
  (let [marker-candidates (partition marker-len 1 signal)
        not-marker? (complement marker?)]
    (count (take-while not-marker? marker-candidates))))

(defn solution1 [[line]] 
  (+ (marker-index packet-marker-len line)
     packet-marker-len)) 

(defn solution2 [[line]] 
  (+ (marker-index message-marker-len line) 
     message-marker-len))

(comment
  ;; 1: 1848
  (util.input/with-input-lines "day_6.txt" solution1)
  ;; 2: 2308
  (util.input/with-input-lines "day_6.txt" solution2)

  (def test-input-1 (vector "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
  (def test-input-2 (vector "bvwbjplbgvbhsrlpgdmjqwftvncz"))
  (def test-input-3 (vector "nppdvjthqldpwncqszvftbrmjlhg"))
  (def test-input-4 (vector "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  (def test-input-5 (vector "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

  (= 7 (solution1 test-input-1))
  (= 5 (solution1 test-input-2))
  (= 6 (solution1 test-input-3))
  (= 10 (solution1 test-input-4))
  (= 11 (solution1 test-input-5))

  (= 19 (solution2 test-input-1))
  (= 23 (solution2 test-input-2))
  (= 23 (solution2 test-input-3))
  (= 29 (solution2 test-input-4))
  (= 26 (solution2 test-input-5)))
