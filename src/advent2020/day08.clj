(ns advent2020.day08
  (:require [advent2020.core :refer [split]]))

(def program
  (->> "resources/day08.txt"
       slurp
       (split #"\n")
       (mapv (fn [s]
               (let [[[_ op sign n]] (re-seq #"(acc|jmp|nop) ([+|-])(\d+)" s)]
                 [op (resolve (read-string sign)) (Integer/parseInt n)])))))


(defn run
  ([program line acc]
   (loop [line line
          acc acc
          lines #{}]
     (if (= line 650)
       [true acc]
       (if (contains? lines line)
         [false acc]
         (let [[op sign n] (program line)]
          (println "[" op sign n "] " "line: " line ", acc: " acc ", lines :" lines)
          (case op
            "jmp" (recur (sign line n) acc (conj lines line))
            "acc" (recur (inc line) (sign acc n) (conj lines line))
            "nop" (recur (inc line) acc (conj lines line))))
        ))))
  ([program] (run program 0 0))
  )

(defn switch [program line]
  (case (first (program line))
    "jmp" (assoc-in program [line 0] "nop")
    "nop" (assoc-in program [line 0] "jmp")
    program))

(doseq [line (range (count program))                        ; line:index of line
        :when (some #(= % (first (program line))) ["jmp" "nop"])
        :let [program* (switch program line)
              result (run program*)]]
  (println "line: " line ", result: " result)
  (if (first result)
    (do (println "The program has terminated normally.")
        (System/exit 0))
    (println "...")))