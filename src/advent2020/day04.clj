(ns advent2020.day04
  (:require [advent2020.core :refer :all]))

;;Day 4
(defn parse-passport [serialized]
  (->> serialized                                           ;"a:1 b:2 c:3"
       (split #" |\n")                                      ;["a:1" "b:2 "c:3"]
       (map #(split #":" %))                                ;[["a" "1"] ["b" "2"] ["c" "3"]]
       (into {})                                            ;{"a" "1", "b" "2", "c" "3"}
       clojure.walk/keywordize-keys))                       ;{:a "1", :b "2", :c "3"}


(defn valid-document? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and byr iyr eyr hgt hcl ecl pid
       (<= 1920 (Integer/parseInt byr) 2002)                ;byr
       (<= 2010 (Integer/parseInt iyr) 2020)                ;iyr
       (<= 2020 (Integer/parseInt eyr) 2030)                ;eyr

       (let [[_ numb unit]                                  ;hgt
             (re-matches #"(\d+)(cm|in)" hgt)]
         (case unit
           "cm" (<= 150 (Integer/parseInt numb) 193)
           "in" (<= 59 (Integer/parseInt numb) 76)
           false))

       (re-matches #"#[0-9a-f]{6}" hcl)                     ;hcl
       (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl) ;ecl
       (re-matches #"[0-9]{9}" pid)                         ;pid
       )
  ) "FBFBBFFRLRFBFBBFFRLR"

(comment
  (->> (slurp "resources/day04.txt")
       (split #"\n\n")
       (map parse-passport)
       (filter valid-document?)
       count))


