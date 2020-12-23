(ns advent2020.day05
  (:require [advent2020.core :refer :all]))

;;Day 5

(defn get-row [rs]
  (loop [letters rs
         [start end] [0 127]]
    (if (seq letters)
      (let [half (/ (- (inc end) start) 2)]
        (case (first letters)
          \F (recur (rest letters) [start (+ start (dec half))])
          \B (recur (rest letters) [(+ start half) end])))
      start))
  )

(defn get-coll [cs]
  (loop [letters cs
         [start end] [0 7]]
    (if (seq letters)
      (let [half (/ (- (inc end) start) 2)]
        (case (first letters)
          \L (recur (rest letters) [start (+ start (dec half))])
          \R (recur (rest letters) [(+ start half) end])))
      start))
  )

;1st loop (input) : letters "FBBFFFB", range [0 127]
;2nd loop: letters "BBFFFB", range [0 63]
;3rd loop: letters "BFFFB", range [32 63]
;4rd loop: letters "FFFB", range [48 63]
;5th loop: letters "FFB", range [48 55]
;6th loop: letters "FB", range [48 51]
;7th loop: letters "B", range [48 49]
;8th loop: letters (), [49 49] -> output: 49

(defn seat-ids [path]
  (->> path
       slurp
       (split #"\n")
       (map #(rest (re-find #"([B|F]{7})([L|R]{3})" %)))
       (map (fn [seat] [(get-row (first seat)) (get-coll (second seat))]))
       (map #(+ (second %) (* 8 (first %))))
       set))

(comment
  ;to get my seat ID
  (clojure.set/difference
    (set (range 96 (inc 911)))
    (seat-ids "resources/day05.txt")))

