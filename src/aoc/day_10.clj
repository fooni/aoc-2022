
(ns aoc.day-10
  (:require util.input
            [clojure.string :as s]))

(defn parse-op [line] 
  (if (s/starts-with? line "noop")
    (vector identity)
    (vector
     identity
     (fn [x] (+ x (->> (s/split line #"\s") 
                              (second) 
                              (util.input/parse-int)))))))

(defn parse-input [lines]
  (mapcat parse-op lines))

;; Task 1
(defn solution1 [lines]
  (let [x-register-vals (reduce (fn [res f]
                                  (conj res (f (peek res))))
                                (vector 1)
                                (parse-input lines))
        signal-strength (map * x-register-vals (drop 1 (range)))
        interesting-signal-strengths (take 6 (take-nth 40 (drop 19 signal-strength)))] 
    (apply + interesting-signal-strengths)))


;; Task 2
(def crt-width 40)
(defn solution2 [lines]
  (let [x-register-vals (reduce (fn [res f]
                                  (conj res (f (peek res))))
                                (vector 1)
                                (parse-input lines))
        crt-ticker (cycle (range 0 crt-width))
        pixel (fn [crt-index x-register-val]
                (if (<= (dec x-register-val) crt-index (inc x-register-val))
                  "#" 
                  "."))
        screen-rows (partition crt-width (map pixel crt-ticker x-register-vals))]
    (run! println (map s/join screen-rows))))

(comment
  ;; 1: 14720
  (util.input/with-input-lines "day_10.txt" solution1)

  ;; 2: FZBPBFZF
  ;; ####.####.###..###..###..####.####.####.
  ;; #.......#.#..#.#..#.#..#.#.......#.#....
  ;; ###....#..###..#..#.###..###....#..###..
  ;; #.....#...#..#.###..#..#.#.....#...#....
  ;; #....#....#..#.#....#..#.#....#....#....
  ;; #....####.###..#....###..#....####.#....
  (util.input/with-input-lines "day_10.txt" solution2)


  (def test-input-1 (seq (s/split-lines "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")))

  (solution1 test-input-1)
  (solution2 test-input-1)
  (= 13140 (solution1 test-input-1))
  )
