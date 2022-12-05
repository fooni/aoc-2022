(ns util.matrix)


(defn transpose
  "Returns a trasposed version of the matrix `mat`"
  [mat]
  (apply map (partial conj []) mat))

(defn rotate-left [mat]
  (apply map (partial conj []) (reverse mat)))