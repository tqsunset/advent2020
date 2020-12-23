(ns advent2020.day06
  (:require [advent2020.core :refer :all]))

;;Day 6
(comment
  ;first problem
  (->> "resources/day06.txt"
       slurp
       (split #"\n\n")
       (map set)
       (map #(disj % \newline))
       (map count)
       (apply +))

  ;second problem
  (->> "resources/day06.txt"
       slurp
       (split #"\n\n")
       (map #(split #"\n" %))
       (map #(map set %))
       (map #(apply clojure.set/intersection %))
       (map count)
       (apply +)
       ))


