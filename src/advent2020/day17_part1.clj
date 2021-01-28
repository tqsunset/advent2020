(ns advent2020.day17-part1
  (:require [advent2020.core :refer :all]))

(def sample ".#.\n..#\n###")

(def input-txt "...#..#.\n.....##.\n##..##.#\n#.#.##..\n#..#.###\n...##.#.\n#..##..#\n.#.#..#.")

(def input-plane
  (let [input (->> input-txt
                   (split #"\n"))
        length (count (first input))
        front (int (/ (- 20 length) 2))
        back (- 20 front length)
        pad-input (->> input
                       (map seq)
                       (map #(concat (repeat front \.) % (repeat back \.))))]
    (->> (concat (repeat front "....................") pad-input (repeat back "...................."))
         (mapv #(vec %)))))

(defn repeatv [n x]
  (->> x
       (repeat n)
       vec))

(def space
  (let [plane (->> \.
                   (repeatv 20)
                   (repeatv 20))]
    (-> (concat (repeat 10 plane) [input-plane] (repeat 10 plane))
        vec)))

;starting plane z= 10

(defn get-cub [space x y z]
  (((space z) y) x))
;valid range
; <= 0 x 19
; <= 0 y 19
; <= 0 z 20

;input space x y z \#
;output updated-space
(defn change [space x y z cube]
  (let [new-line (assoc ((space z) y) x cube)
        new-plane (assoc (space z) y new-line)]
    (assoc space z new-plane)))

;input 2 10 9
;output [[

(defn adj [a b c]
  (for [x [(dec a) a (inc a)]
        y [(dec b) b (inc b)]
        z [(dec c) c (inc c)]
        :when (and (<= 0 x (dec (count (first (first space)))))
                   (<= 0 y (dec (count (first space))))
                   (<= 0 z (dec (count space)))
                   (not (and (= x a) (= y b) (= z c))))]
    [x y z]))

;input space x y z
;output updated-space
(defn update [space x y z]
  (let [cube (get-cub space x y z)
        adj-cubs (map #(apply get-cub space %) (adj x y z))]
    (cond
      (and (= cube \#)
           (not (<= 2 (count (filter #(= % \#) adj-cubs)) 3))) \.
      (and (= cube \.)
           (= 3 (count (filter #(= % \#) adj-cubs)))) \#
      :else cube)))

(defn update-space [space]
  (vec (for [zi (range (count space))
             :let [z (space zi)]]
         (vec (for [yi (range (count (first space)))
                    :let [y (z yi)]]
                (vec (for [xi (range (count (first (first space))))
                           :let [x (y xi)]]
                       (update space xi yi zi))))))))

(->> (loop [space space
            i 0]
       (if (= i 6)
         space
         (recur (update-space space) (inc i))))
     flatten
     (filter #(= % \#))
     count)









