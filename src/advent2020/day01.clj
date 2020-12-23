(ns advent2020.day01
  (:require [clojure.string :as string]))

;;Day 1-1
(defn read-numbers [path]
  (->> (slurp path)
       (string/split-lines)
       (map #(Integer/parseInt %))
       set))

(defn day0101 [sum numbs]
  "finds numbers in numbs that sums into sum"
  (clojure.set/intersection
    (set numbs)
    (set (map #(- sum %) numbs))))

; A+B = 2020 - C
; my solulution (36.99 msec)
(let [numbs (read-numbers "resources/day01.txt")
      right (map #(- 2020 %) numbs)]
  (apply * (apply clojure.set/union (filter (complement empty?) (map #(day0101 % numbs) right)))))

; solution with for - more efficient (0.82 msec)
(let [numbs (read-numbers "resources/day01.txt")]
  (for [a numbs
        b numbs
        :let [c (- 2020 a b)]
        :when (< a b c)
        :when (contains? numbs c)]
    [(* a b c)]))
