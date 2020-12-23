(ns advent2020.day07
  (:require [advent2020.core :refer :all]))

;;Day 07

;input: "plaid salmon bags contain 5 dull fuchsia bags, 4 faded gold bags, 2 dark teal bags, 1 bright teal bag."
;output: {"plaid salmon" {"dull fuchsia" 5, "faded gold" 4, "dark teal" 2, "bright teal" 1}}
(defn parse-bag [line]
  (let [[[_ _ _ container] & elements]
        (re-seq #"(^|(\d+) )(\w+ \w+) bag?" line)]
    {container (into {} (mapv (fn [[_ _ n color]] [color (Integer/parseInt n)]) elements))}
    ))

(defn map-bag [file]
  (->> file
       slurp
       (split #"\n")
       (map parse-bag)
       (apply merge)
       ))

(defn union [& sets]
  (apply clojure.set/union sets)
  )

;input #{"shiny gold"}
;output #{"muted yellow" "bright white"}
(defn find-containers [elements]
  (apply union
         (map (fn [element]
                (let [rules (map-bag "resources/day07.txt")]
                  (->> (for [container (keys rules)]
                         (when (contains? (get rules container) element)
                           container))
                       (filter (complement nil?))
                       set)))
              elements)))

(defn find-all-containers [bag]
  (loop [elements #{bag}
         acc #{}]
    (if (seq elements)
      (let [containers (find-containers elements)]
        (recur containers (union acc containers)))
      acc)))

(comment
  (count (find-all-containers "shiny gold")))               ;first question

(defn container? [bag]
  (let [rules (map-bag "resources/day07.txt")
        upper-container (reduce
                          (fn [acc [container elements]]
                            (if (contains? elements bag)
                              (conj acc container)
                              acc)) [] rules)]
    (flatten (apply conj upper-container (map container? upper-container)))
    ))

(comment
  (count (set (container? "shiny gold"))))                  ;first question-02

;to find sets of smallest bags
(def sm-bags (let [rules (map-bag "resources/day07.txt")]
               (set (keys (filter (fn [[container value]]
                                    (not (seq value))) rules)))))

;input {"shiny gold" 2}
;output {"dark red" 4}
;input2 {"dark blue" 2 "dark violet" 2}
;output2 {"dark violet" 6}
(defn find-elements [containers]
  (apply merge
         (map (fn [[container n]]
                (if (contains? sm-bags container)
                  {container n}
                  (let [rules (map-bag "resources/day07.txt")]
                    (multiply-value (get rules container) n)))) containers)))

(defn find-total-elements [bag]
  (loop [containers {bag 1}
         acc {}]
    (if (apply = true (map #(contains? sm-bags %) (keys containers)))
      acc
      (let [elements (find-elements containers)]
        (println elements "\n\\\\\n" (merge acc elements) "\n")
        (recur elements (merge acc elements))))))

(comment
  (apply + (vals (find-total-elements "shiny gold"))))

(defn sub-count [container]
  (let [rules (map-bag "resources/day07.txt")]
    (->> container
         rules
         (reduce
           (fn [acc [color n]]
             (+ acc n (* (sub-count color) n)))
           0))))





