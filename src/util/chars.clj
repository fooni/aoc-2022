(ns util.chars)



(defn char-range [from to]
  (map char (range (int from) (inc (int to)))))