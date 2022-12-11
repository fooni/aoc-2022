
(ns aoc.day-7
  (:require util.input
            [clojure.string :as s]))

(def filesystem-root-path "/")
(def filesystem-root {filesystem-root-path {}})

(defn command-line? [line] (s/starts-with? line "$"))
(def result-line? (complement command-line?))
(defn parse-commands
  ([lines] (parse-commands [] (seq lines)))
  ([commands lines]
   (if (seq lines)
     (let [[_ cmd & params] (s/split (first lines) #"\s+")
           [result rest] (split-with result-line? (next lines))
           command {:cmd (keyword cmd) :params params :results result}]
       (parse-commands (conj commands command) rest))
     commands)))


(defn discover-files [filesystem ls-result-lines] 
  (reduce (fn add-file [filesystem ls-line] 
            (let [[a b] (s/split ls-line #" ")]
              (if (= a "dir")
                (assoc filesystem b {})
                (assoc filesystem b (util.input/parse-int a)))))
          filesystem
          ls-result-lines))

(defn probe-filesystem [[path filesystem] {:keys [cmd params results]}]
  (case cmd
    :cd (let [path-param (first params)] 
          (case path-param
            "/" [[filesystem-root-path] filesystem]
            ".." [(vec (butlast path)) filesystem]
            [(conj path path-param) filesystem]))
    :ls [path (update-in filesystem path #(discover-files % results))]))

(defn parse-filesystem [lines]
  (let [commands (parse-commands lines)
        inital-state [[filesystem-root-path] filesystem-root]]
    (second (reduce probe-filesystem inital-state commands))))

(def filesystem-branch? map?)
(defn filesystem-children [node] (map (fn [[_ v :as n]] (if (map? v) v n)) node))
(def filesystem-tree-seq (partial tree-seq filesystem-branch? filesystem-children))

(def file? vector?)
(def dir? map?)

(defn files [filesystem-node] (filter file? (filesystem-tree-seq filesystem-node)))
(defn dirs [filesystem-node] (filter dir? (filesystem-tree-seq filesystem-node)))

(def file-size second)
(defn dir-size [dir]
  (->> (filesystem-tree-seq dir)
       (filter file?)
       (map file-size)
       (apply +)))

;; Task 1
(def max-dir-threshold 100000)

(defn solution1 [lines] 
  (->> (parse-filesystem lines)
       (dirs)
       (map dir-size)
       (filter #(<= % max-dir-threshold))
       (apply +)))


;; Task 2
(def total-diskspace 70000000)
(def update-size 30000000)

(defn solution2 [lines] 
  (let [filesystem (parse-filesystem lines)
        used-diskspace (dir-size filesystem)
        free-diskspace (- total-diskspace used-diskspace)
        required-diskspace (- update-size free-diskspace)]
    (when (pos? required-diskspace)
      (->> (dirs filesystem)
           (map dir-size)
           (sort)
           (drop-while #(< % required-diskspace))
           (first)))))

(comment
  ;; 1: 1648397
  (util.input/with-input-lines "day_7.txt" solution1)
  ;; 2: 1815525
  (util.input/with-input-lines "day_7.txt" solution2)

  (def test-input-1 (seq (s/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd as
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")))
  
  (= 95437 (solution1 test-input-1))
  (= 24933642 (solution2 test-input-1))
  )
