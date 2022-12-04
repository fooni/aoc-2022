(ns util.input
  (:require [clojure.java.io :as io]))

(defn with-input-lines [path f]
  (let [file (io/resource path)]
    (with-open [reader (io/reader file)] 
      (f (line-seq reader)))))

(defn parse-int
  "Parse string or character to integer"
  ([s] (parse-int s 10))
  ([s base] (Integer/parseInt (str s) base)))