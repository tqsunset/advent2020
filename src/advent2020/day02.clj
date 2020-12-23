(ns advent2020.day02
  (:require [advent2020.core :refer :all]))

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


