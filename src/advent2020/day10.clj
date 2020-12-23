(ns advent2020.day10
  (:require [advent2020.core :refer [split]]))

(defn times [x n]
  (apply * (repeat n x)))

(def adapters
  (let [parsed-text (->> "resources/day10.txt"
                   slurp
                   (split #"\n")
                   (map #(Integer/parseInt %)))]
    (conj parsed-text 0 (+ 3 (apply max parsed-text)))))

(def jol-differs
  (->> adapters
       sort
       (partition 2 1)
       (map #(apply - (reverse %)))))

(comment
  (let [{one 1, three 3} (frequencies jol-differs)]
    (* one three)
    )

  (let [{fours '(1 1 1 1), threes '(1 1 1), twos '(1 1), ones '(1)}
        (->> (partition-by #(= % 3) jol-differs)
             (keep-indexed #(when (even? %1) %2))
             frequencies)]
    (* (times 7 fours) (times 4 threes) (times 2 twos))))

[1 1 1 1 1]
(defn count-ways
  ([x] 1)
  ([x y & zs]))