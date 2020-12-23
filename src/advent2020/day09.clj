(ns advent2020.day09
  (:require [advent2020.core :refer [split]]))

(def numbers
  (->> "resources/day09.txt"
           slurp
           (split #"\n")
           (mapv #(Long/parseLong %))))

(defn valid-number? [numb coll]
  (let [s-numbs (set coll)
        s-numbs* (set (map #(- numb %) coll))]
    (seq (clojure.set/intersection s-numbs s-numbs*))))

(comment
  (doseq [i (range 25 (inc (count numbers)))]
    (if (valid-number? (numbers i) (drop (- i 25) (take i numbers)))
      (println (numbers i) "is valid.")
      (do (println (numbers i) "is not valid.")
          (System/exit 0))))                                ; 18272118

  (doseq [i (range 2 100)
          group (set (partition i 1 numbers))]
    (if (= (apply + group) 18272118)
      (do (println group " is valid.")
          group
          (System/exit 0))
      (println group "is not valid.")))

  (let [weakness '(720149 731033 1466212 1388501 1349179 816228 923226 973294 1004417 1122441 948607 979730 1398951 1038383 1079070 1116758 1215939)
        max (apply max weakness)
        min (apply min weakness)]
    (+ min max)
    ))