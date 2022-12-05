
(ns aoc.day-5
  (:require [util.input]
            [util.matrix :as mat]
            [clojure.string :as s]))

(defn parse-input [lines]
  (let [space? (partial = \space)
        not-space? (complement space?)
        not-empty? (complement empty?)
        crates (->> (take-while not-empty? lines)
                    (mat/rotate-left)
                    (remove #(space? (first %)))
                    (map #(take-while not-space? (drop 1 %))))
        moves (->> (drop-while not-empty? lines)
                   (drop-while empty?)
                   (map #(re-seq #"\d+" %))
                   (map #(map util.input/parse-int %)))]
    (vector crates moves)))

(defn crane [lifter-fn]
  (fn move-crates [crates [n from to]]
    (let [source-stack-idx (dec from)
          target-stack-idx (dec to)
          source-stack (nth crates source-stack-idx)
          moved-crates (lifter-fn (take-last n source-stack))
          lift (fn lift [c] (update-in (vec c) [source-stack-idx] #(drop-last n %)))
          drop (fn drop [c] (update-in (vec c) [target-stack-idx] #(concat % moved-crates)))
          lift-n-drop (comp drop lift)]
      (lift-n-drop crates))))

(def crane-9000 (crane reverse))
(def crane-9001 (crane identity))

(defn top-crates [crates moves crane]
  (->> moves
       (reduce crane crates)
       (map last)
       (apply str)))

(defn solution1 [lines]
  (let [[crates moves] (parse-input lines)] 
    (apply str (top-crates crates moves crane-9000))))

(defn solution2 [lines] 
  (let [[crates moves] (parse-input lines)]
    (apply str (top-crates crates moves crane-9001))))


(comment
  ;; 1: GRTSWNJHH
  (util.input/with-input-lines "day_5.txt" solution1)
  ;; 2: QLFQDBBHM
  (util.input/with-input-lines "day_5.txt" solution2)
  
  (def test-input-1 (seq (s/split-lines "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"))) 
  
  (= "CMZ" (solution1 test-input-1 ))
  (= "MCD" (solution2 test-input-1 )))
