
(ns aoc.day-8
  (:require util.input
            [util.matrix :refer [rotate-left rotate-right]]
            [clojure.string :as s]))

(def or-2 #(or %1 %2))

(defn visible-left [tree-row]
 (second (reduce (fn [[highest res] tree]
                   (vector 
                    (max highest tree)
                    (conj res (< highest tree))))
                 [-1 []]
                 tree-row)))
(def visible-right (comp vec reverse visible-left reverse))

(defn visible-both [tree-row]
  (map or-2
       (visible-left tree-row)
       (visible-right tree-row)))

(def visible-x (partial map visible-both))
(def visible-y (comp rotate-right visible-x rotate-left))

(defn visible [trees] 
  (map #(map or-2 %1 %2) 
       (visible-x trees) 
       (visible-y trees)))

(defn parse-input [lines]
  (map (partial map util.input/parse-int) lines))

;; Task 1
(defn solution1 [lines]
  (let [tree-mat (parse-input lines)
        visible-mat (visible tree-mat)]
    (->> (flatten visible-mat)
         (filter true?)
         (count))))


;; Task 2
(defn view-dist-left [tree-row] 
  (map (fn view-dist [[tree & rest]]
         (if rest
           (let [[shorter taller] (split-with #(< % tree) rest)]
             (+ (count shorter) (if (seq taller) 1 0)))
           0))
       (take-while some? (iterate next tree-row))))
(def view-dist-right (comp vec reverse view-dist-left reverse))
(defn view-dist-both [tree-row]
  (map *
       (view-dist-left tree-row)
       (view-dist-right tree-row)))

(def view-dist-x (partial map view-dist-both))
(def view-dist-y (comp rotate-right view-dist-x rotate-left))

(defn view-dist [trees]
  (map #(map * %1 %2)
       (view-dist-x trees)
       (view-dist-y trees)))

(defn solution2 [lines]
  (apply max (flatten (view-dist (parse-input lines)))))

(comment
  ;; 1: 1543
  (util.input/with-input-lines "day_8.txt" solution1)
  ;; 2: 595080
  (util.input/with-input-lines "day_8.txt" solution2)
  
  
  (def test-input-1 (seq (s/split-lines "30373
25512
65332
33549
35390"))) 
  
  (= 21 (solution1 test-input-1))
  (= 8 (solution2 test-input-1)))
