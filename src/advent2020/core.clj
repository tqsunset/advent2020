(ns advent2020.core
  (:require [clojure.string :as string]))

(defn listify-txt [path]
  (string/split-lines (slurp path)))

;;Day 1-1
(defn list-numb [path]
  (map read-string (listify-txt path)))

(defn day0101 [sum numbs]
  "finds numbers in numbs that sums into sum"
  (clojure.set/intersection
    (set numbs)
    (set (map #(- sum %) numbs))))

;Day 1-2 :A+B = 2020 - C
(let [numbs (list-numb "resources/day01.txt")
      right (map #(- 2020 %) numbs)]
  (apply * (apply clojure.set/union (filter (complement empty?) (map #(day0101 % numbs) right)))))

;;Day 2-1
(defn password-old? [min max character password]
  (let [number (count (filter #(= % character) password))]
    (<= min number max)
    ))

(defn parse-passlist [path]
  (->> (map #(string/split % #"\s|\-") (listify-txt path))
       (map #(update % 0 read-string))
       (map #(update % 1 read-string))
       (map (fn [n] (update n 2 #(first %))))))

(defn count-valid-pass [predicate path]
  (let [passlist (parse-passlist path)]
    (->> (map #(apply predicate %) passlist)
         (filter true?)
         count)))

;(count-valid-pass password-old? "resources/day02.txt")

;;Day 2-2
(defn password-new? [pos1 pos2 character password]
  (let [p (= character (nth password (dec pos1)))
        q (= character (nth password (dec pos2)))
        cond1 (and (not p) q)
        cond2 (and p (not q))]
    (when (or cond1 cond2)
      true)
    ))

;(count-valid-pass password-new? "resources/day02.txt")

;;Day 3
(def pattern (vec (listify-txt "resources/day03.txt")))

(defn listify-coordinate [pattern right down]
  (let [height (count pattern)
        width (count (first pattern))]
    (for [i (range 400)
          :let [x (inc (* right i))
                y (inc (* down i))]
          :when (<= y height)]
      [x y])))

(defn get-point [pattern [x y]]
  (let [width (count (first pattern))
        x' (mod x width)]
    (-> (get pattern (dec y))
        (get (dec (if (= 0 x') width x'))))))

(defn map-path [pattern right down]
  (map #(get-point pattern %) (listify-coordinate pattern right down)))

(defn count-tree-on-path [pattern right down]
  (->> (map-path pattern right down)
       (filter #(= % \#))
       count))

(comment
  (count-tree-on-path pattern 3 1)
  (->> (map #(apply count-tree-on-path pattern %) [[1 1] [3 1] [5 1] [7 1] [1 2]])
       (apply *)))
