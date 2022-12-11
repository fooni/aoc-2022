
(ns aoc.day-9
  (:require util.input
            [clojure.string :as s]))


(defn point [x y] (vector x y))
(def x first)
(def y second)
(defn delta [point-1 point-2]
  (point (- (x point-1) (x point-2))
         (- (y point-1) (y point-2))))

(def origin (point 0 0))

(defn parse-input [lines]
  (mapcat (fn [line]
            (let [[direction times] (s/split line #"\s")]
              (vec (repeat (util.input/parse-int times)
                           (keyword direction)))))
          lines))

(defn move [point direction]
  (let [x (x point)
        y (y point)]
    (case direction
      :U (vector x (inc y))
      :D (vector x (dec y))
      :L (vector (dec x) y)
      :R (vector (inc x) y)
      :UR (vector (inc x) (inc y))
      :UL (vector (dec x) (inc y))
      :DR (vector (inc x) (dec y))
      :DL (vector (dec x) (dec y)))))

(defn move-tail [head tail] 
  (case (delta head tail)
    ([0 2]) (move tail :U)
    ([0 -2]) (move tail :D)
    ([-2 0]) (move tail :L)
    ([2 0]) (move tail :R)
    ([-2 1] [-1 2] [-2 2]) (move tail :UL)
    ([2 1] [1 2] [2 2]) (move tail :UR)
    ([-2 -1] [-1 -2] [-2 -2]) (move tail :DL)
    ([2 -1] [1 -2] [2 -2]) (move tail :DR)
    tail))

(defn head-positions [head-movements]
  (reduce (fn [res direction]
            (conj res (move (peek res) direction)))
          (vector origin)
          head-movements))

(defn tail-positions [head-positions] 
  (reduce (fn [res head-pos]
            (conj res (move-tail head-pos (peek res))))
          (vector (first head-positions))
          (rest head-positions)))

;; Task 1
(defn solution1 [lines]
  (let [moves (parse-input lines)
        head-positions (head-positions moves)
        tail-positions (tail-positions head-positions)]
    (count (set tail-positions))))

;; Task 2
(defn solution2 [lines]
  (let [moves (parse-input lines)
        head-positions (head-positions moves)
        tail-pos-seq (iterate tail-positions (tail-positions head-positions))]
    (count (set (nth tail-pos-seq 8)))
    ))

(comment
  ;; 1: 5779
  (util.input/with-input-lines "day_9.txt" solution1)
  ;; 2: 2331
  (util.input/with-input-lines "day_9.txt" solution2)

  (def test-input-1 (seq (s/split-lines "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")))
  (= 13 (solution1 test-input-1))

  (def test-input-2 (seq (s/split-lines "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")))
  (= 36 (solution2 test-input-2)) 
  )
