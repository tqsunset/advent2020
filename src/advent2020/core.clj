(ns advent2020.core
  (:require [clojure.string :as string]))

(defn listify-txt [path]
  (string/split-lines (slurp path)))

;;Day 1-1
(defn read-numbers [path]
  (->> (slurp path)
       (string/split-lines)
       (map #(Integer/parseInt %))
       set))

(defn day0101 [sum numbs]
  "finds numbers in numbs that sums into sum"
  (clojure.set/intersection
    (set numbs)
    (set (map #(- sum %) numbs))))

; A+B = 2020 - C
; my solulution (36.99 msec)
(let [numbs (read-numbers "resources/day01.txt")
      right (map #(- 2020 %) numbs)]
  (apply * (apply clojure.set/union (filter (complement empty?) (map #(day0101 % numbs) right)))))

; solution with for - more efficient (0.82 msec)
(let [numbs (read-numbers "resources/day01.txt")]
  (for [a numbs
        b numbs
        :let [c (- 2020 a b)]
        :when (< a b c)
        :when (contains? numbs c)]
    [(* a b c)]))

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

;;Day 3
(def pattern (vec (listify-txt "resources/day03.txt")))

(defn listify-coordinate [pattern right down]
  (let [height (count pattern)
        width (count (first pattern))]
    (for [i (range 400)
          :let [x (inc (* right i))
                y (inc (* down i))]
          :when (<= y height)]
      [x y])))

(defn get-point [pattern [x y]]
  (let [width (count (first pattern))
        x' (mod x width)]
    (-> (get pattern (dec y))
        (get (dec (if (= 0 x') width x'))))))

(defn map-path [pattern right down]
  (map #(get-point pattern %) (listify-coordinate pattern right down)))

(defn count-tree-on-path [pattern right down]
  (->> (map-path pattern right down)
       (filter #(= % \#))
       count))

(comment
  (count-tree-on-path pattern 3 1)
  (->> (map #(apply count-tree-on-path pattern %) [[1 1] [3 1] [5 1] [7 1] [1 2]])
       (apply *)))


;;Day 4
(defn split [regex s]
  (string/split s regex))

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
  )"FBFBBFFRLRFBFBBFFRLR"

(comment
  (->> (slurp "resources/day04.txt")
       (split #"\n\n")
       (map parse-passport)
       (filter valid-document?)
       count))


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