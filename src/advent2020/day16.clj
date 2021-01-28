(ns advent2020.day16
  (:require [advent2020.core :refer :all]))

(defn parse [str]
  (Integer/parseInt str))

(def sample "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")

(def input (slurp "resources/day16.txt"))

(let [[range-str my-str other-str] (split #"\n\n" input)]
  (def field-range
    (->> (for [[[_ field n1 n2 n3 n4]] (->> range-str
                                            (split #"\n")
                                            (map #(re-seq #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)" %)))]
           [(keyword (clojure.string/replace field #" " "-")) #(or (<= (parse n1) % (parse n2)) (<= (parse n3) % (parse n4)))])
         (into {})))

  (def my-ticket
    (->> my-str
         (re-seq #"\d+")
         (mapv parse)))

  (def tickets
    (->> other-str
         (split #"\n")
         rest
         (mapv #(->> (re-seq #"\d+" %)
                     (mapv parse))))))

;input [#(<= 0 % 10) #(<= 44 % 48)] [1 11 14 46]
;output [11 14]
(defn find-invalid [preds numbs]
  (loop [numbs numbs
         acc []]
    (if (seq numbs)
      (if (->> (first numbs)
               ((apply juxt preds))
               (some true?))
        (recur (rest numbs) acc)
        (recur (rest numbs) (conj acc (first numbs))))
      acc)))

(defn error-rate [field-range tickets]
  (->> (reduce (fn [acc ticket] (into acc (find-invalid (vals field-range) ticket)))
               [] tickets)
       (apply +)))

(def v-tickets
  (reduce (fn [acc ticket]
            (if (empty? (find-invalid (vals field-range) ticket))
              (conj acc ticket)
              acc))
          [] tickets))

(def by-fields
  (apply mapv vector v-tickets))

;input {:row 1 :class 0} [2 '(:class :row :seat)]
;output {:row 1 :class 0 :seat 2}
(defn find-valid-field [m [ind fields]]
  (->> (concat (remove m fields) [ind])
       vec
       (apply assoc m)))


(->>
  (for [i (range (count by-fields))
        :let [values (by-fields i)]]
    [i (keep (fn [[field rang]] (when (every? true? (map rang values)) field)) field-range)]) ;=> ([0 (:row)] [1 (:class :row)] [2 (:class :row :seat)])
  (sort-by (fn [[i coll]] (count coll)))
  (reduce (fn [acc x] (find-valid-field acc x)) {})

  (reduce (fn [acc [field ind]] (if (re-find #"departure" (str field))
                                  (conj acc ind)
                                  acc)) [])
  (map (fn [ind] (my-ticket ind)))
  (apply *)
  )

