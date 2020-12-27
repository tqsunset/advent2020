(ns advent2020.day12
  (:require [advent2020.core :refer :all]))

(def instructions
  (->> "resources/day12.txt"
       slurp
       (split #"\n")
       (map #(re-seq #"([A-Z])(\d+)" %))
       (mapv (fn [[[_ dir n]]] [dir (Integer/parseInt n)]))))

(defn turn-right [ship-dir n]
  (let [d ({"N" 0 "E" 1 "S" 2 "W" 3} ship-dir)
        n (quot n 90)]
    (["N" "E" "S" "W"] (rem (+ d n) 4))))

(defn step [[ship-dir x y] instruction]
  (let [n (second instruction)]
    (case (first instruction)
      "N" [ship-dir x (+ y n)]
      "S" [ship-dir x (- y n)]
      "E" [ship-dir (+ x n) y]
      "W" [ship-dir (- x n) y]
      "R" [(turn-right ship-dir n) x y]
      "L" [(turn-right ship-dir (- 360 n)) x y]
      "F" (step [ship-dir x y] [ship-dir n]))))

(comment                                                    ;part 1
  (loop [status ["E" 0 0]
         i 0]
    (if (< i (count instructions))
      (do
        (println "current :" status ", instruction :" (instructions i))
        (recur (step status (instructions i)) (inc i)))
      status)))

(defn way-right [[wx wy] n]
  (reduce (fn [[x y] t]
            [y (- x)])
          [wx wy] (repeat n 1)))

(defn step* [[[x y] [wx wy]] instruction]
  (let [n (second instruction)]
    (case (first instruction)
      "N" [[x y] [wx (+ wy n)]]
      "S" [[x y] [wx (- wy n)]]
      "E" [[x y] [(+ wx n) wy]]
      "W" [[x y] [(- wx n) wy]]
      "R" [[x y] (way-right [wx wy] (quot n 90))]
      "L" [[x y] (way-right [wx wy] (- 4 (quot n 90)))]
      "F" [(mapv + [x y] (map #(* % n) [wx wy])) [wx wy]])))

(comment                                                    ;part 1
  (let [result (loop [status [[0 0] [10 1]]
                      i 0]
                 (if (< i (count instructions))
                   (do
                     (println "current location & waypoint :" status ", instruction :" (instructions i))
                     (recur (step* status (instructions i)) (inc i)))
                   status))]
    (->> (first result)
         (map abs)
         (apply +))))