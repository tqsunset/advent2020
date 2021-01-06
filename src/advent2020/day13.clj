(ns advent2020.day13
  (:require [advent2020.core :refer :all]))

(def bus-data
  (->> "resources/day13.txt"
       slurp
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       ((fn [[n & xs]] {:timestamp n :bus-numb xs}))))

(defn time-left [timestamp bus-numb]
  (- (* (inc (quot timestamp bus-numb)) bus-numb) timestamp))

(comment
  (let [result (zipmap (bus-data :bus-numb) (map #(time-left (bus-data :timestamp) %) (bus-data :bus-numb)))]
    (->> (apply min-key val result)
         (apply *))))                                       ;part 1

(def bus-data*
  (let [numbers (->> "resources/day13.txt"
                     slurp
                     (re-seq #"x|\d+")
                     rest)]
    (->> (zipmap (range (count numbers)) numbers)
         (remove (fn [[t n]] (= n "x")))
         (map (fn [[t n]] [t (Integer/parseInt n)])))))

(defn magic [[sum plus] [delay bus-numb]]
  (loop [sum sum]
    (if (zero? (mod (+ sum delay) bus-numb))
      [sum (* plus bus-numb)]
      (recur (+ sum plus)))))

(comment
  (reduce magic bus-data*))                                 ;part 2
