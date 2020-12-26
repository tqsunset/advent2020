(ns advent2020.day11
  (:require [advent2020.core :refer :all]))

(def seat-ori
  (->> "resources/day11.txt"
       slurp
       (split #"\n")
       (mapv vec)))

(defn get-seat [seat-map row coll]
  ((seat-map row) coll))

(defn adj [seat-map row coll]
  "return seq of adjacent seat"
  (for [r [(dec row) row (inc row)]
        c [(dec coll) coll (inc coll)]
        :when (and (<= 0 r (dec (count seat-map)))
                   (<= 0 c (dec (count (seat-map 1))))
                   (not (and (= r row) (= c coll))))]
    (get-seat seat-map r c)))

(defn change-seat [seat-map row coll]
  "get location of the seat and seat map, return the changed seat according to the part 1 rule"
  (let [seat (get-seat seat-map row coll)
        adj-seats (adj seat-map row coll)]
    (case seat
      \L (if (every? #(or (= % \L) (= % \.)) adj-seats)
           \#
           \L)
      \# (if (<= 4 (count (filter #(= % \#) adj-seats)))
           \L
           \#)
      \. \.)))

(defn change-map [seat-map]
  (vec (for [r (range (count seat-map))]
         (vec (for [c (range (count (seat-map 1)))]
                (change-seat seat-map r c))))))

(defn get-final-map [seat-map]
  (loop [seat-map seat-map]
    (if (= seat-map (change-map seat-map))
      seat-map
      (recur (change-map seat-map)))))

(def final-map (get-final-map seat-ori))

(->> final-map
     (map (fn [s] (filter #(= % \#) s)))
     (map count)
     (apply +))                                             ;Part 1

(def direction {:W  [0 -1]
                :E  [0 1]
                :N  [-1 0]
                :S  [1 0]
                :SW [1 -1]
                :NW [-1 -1]
                :SE [1 1]
                :NE [-1 1]})

(defn visible-seat [seat-map dir row coll]
  {:pre [(<= 0 row (dec (count seat-ori))) (<= 0 coll (dec (count (seat-ori 1))))]}
  (let [max-row (dec (count seat-ori))
        max-coll (dec (count (seat-ori 1)))]
    (loop [[r c] (map + (direction dir) [row coll])]
      (if (and (<= 0 r max-row) (<= 0 c max-coll))
        (case (get-seat seat-map r c)
          \. (recur (map + (direction dir) [r c]))
          \# \#
          \L \L)
        \.))))

(defn visible-seats [seat-map row coll]
  (for [dir (keys direction)]
    (visible-seat seat-map dir row coll)))

(defn change-seat* [seat-map row coll]
  "get location of the seat and seat map, return the changed seat according to the part 2 rule"
  (let [seat (get-seat seat-map row coll)
        visible (visible-seats seat-map row coll)]
    (case seat
      \L (if (every? #(or (= % \L) (= % \.)) visible)
           \#
           \L)
      \# (if (<= 5 (count (filter #(= % \#) visible)))
           \L
           \#)
      \. \.)))


(defn change-map* [seat-map]
  (vec (for [r (range (count seat-map))]
         (vec (for [c (range (count (seat-map 1)))]
                (change-seat* seat-map r c))))))

(defn get-final-map* [seat-map]
  (loop [seat-map seat-map]
    (if (= seat-map (change-map* seat-map))
      seat-map
      (recur (change-map* seat-map)))))

(def final-map (get-final-map* seat-ori))

(->> final-map
     (map (fn [s] (filter #(= % \#) s)))
     (map count)
     (apply +))