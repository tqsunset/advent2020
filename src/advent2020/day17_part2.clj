(ns advent2020.day17-part2
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

(def input-space
  (let [plane (->> \.
                   (repeatv 20)
                   (repeatv 20))]
    (-> (concat (repeat 10 plane) [input-plane] (repeat 10 plane))
        vec)))

(def space4
  (let [space (->> \.
                   (repeatv 20)
                   (repeatv 20)
                   (repeatv 20))]
    (-> (concat (repeat 10 space) [input-space] (repeat 10 space))
        vec)))

(defn get-cub [space4 x y z w]
  ((((space4 w) z) y) x))

;input space x y z w \#
;output updated-space
(defn change [space4 x y z w cube]
  (let [new-line (assoc (((space4 w) z) y) x cube)
        new-plane (assoc ((space4 w) z) y new-line)
        new-space (assoc (space4 w) z new-plane)]
    (assoc space4 w new-space)))

(defn adj [a b c d]
  (for [x [(dec a) a (inc a)]
        y [(dec b) b (inc b)]
        z [(dec c) c (inc c)]
        w [(dec d) d (inc d)]
        :when (and (<= 0 x (dec (count (first (first (first space4))))))
                   (<= 0 y (dec (count (first (first space4)))))
                   (<= 0 z (dec (count (first space4))))
                   (<= 0 w (dec (count space4)))
                   (not (and (= x a) (= y b) (= z c) (= w d))))]
    [x y z w]))

;input space x y z w
;output updated-space
(defn update [space4 x y z w]
  (let [cube (get-cub space4 x y z w)
        adj-cubs (map #(apply get-cub space4 %) (adj x y z w))]
    (cond
      (and (= cube \#)
           (not (<= 2 (count (filter #(= % \#) adj-cubs)) 3))) \.
      (and (= cube \.)
           (= 3 (count (filter #(= % \#) adj-cubs)))) \#
      :else cube)))

(defn update-space4 [space4]
  (vec (for [wi (range (count space4))
             :let [w (space4 wi)]]
         (vec (for [zi (range (count (first space4)))
                    :let [z (w zi)]]
                (vec (for [yi (range (count (first (first space4))))
                           :let [y (z yi)]]
                       (vec (for [xi (range (count (first (first (first space4)))))
                                  :let [x (y xi)]]
                              (update space4 xi yi zi wi))))))))))

(comment
  (->> (loop [space4 space4
             i 0]
        (if (= i 6)
          space4
          (recur (update-space4 space4) (inc i))))
      flatten
      (filter #(= % \#))
      count))









