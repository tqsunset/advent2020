(ns advent2020.day03
  (:require [advent2020.core :refer :all]))

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

