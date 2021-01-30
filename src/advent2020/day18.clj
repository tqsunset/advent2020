(ns advent2020.day18
  (:require [advent2020.core :refer :all]))

(defn linear-helper [operand operator rests]
  (if (= (count rests) 1)
    (if (list? (first rests))
      (list operator operand (linear (first rests)))
      (list operator operand (first rests)))
    (list operator operand (linear (reverse rests)))))

;input ("7" "+" "4" "*" "5")
;output ("*" ("+" "7" "4") "5")
(defn linear [input]
  (loop [[operand operator & rests] (reverse input)]
    (if (list? operand)
      (linear-helper (linear operand) operator rests)
      (linear-helper operand operator rests))))

(linear (read-string "(7 + (4 * 5) * 2)"))
(linear (read-string "(7 + 4 * ((3 + 3) * 5) * 2)"))
(linear (read-string "(4 * (3 * 5))"))
(linear (read-string "(3 * 5)"))

(->> "resources/day18.txt"
     slurp
     (split #"\n")
     (map #(str "(" % ")"))
     (map #(eval (linear (read-string %))))
     (apply +))

















;tree-seq


;input ("7" "+" "(" "4" "*" "5" ")")
;output ("7" "+" ("4" "*" "5"))
(let [input '("7" "+" "(" "4" "*" "5" ")")]
  (partition-by #(or (= % "(") (= % ")")) input))

;input "(7 + (4 * 5)) * 2"
;output (("7" "+" ("4" "*" "5" )) "*" "2")

(let [input-string "3 + (7 + (4 * 5)) * 2"]
  (re-seq #"\(([\w+* ()]+)\)" input-string))
