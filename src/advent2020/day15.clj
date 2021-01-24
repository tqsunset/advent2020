(ns advent2020.day15
  (:require [advent2020.core :refer :all]))


;; 1st Way :using vector

(def starting
  (->> "3,2,1"
       (split #",")
       (mapv #(Integer/parseInt %))))

(defn age [coll n]
  (->> (keep-indexed #(when (= %2 n) %1) coll)
       (take-last 2)
       reverse
       (apply -)))

(comment
  (time
    (loop [input starting
           i (count input)]
      (let [last-numb (last input)
            age-numb (age input last-numb)]
        (if (= i 2020)
          last-numb
          (if (pos? age-numb)
            (recur (conj input age-numb) (inc i))
            (recur (conj input 0) (inc i)))))))             ; "Elapsed time: 352.260499 msecs"


  (time
    (loop [input starting]
      (let [last-numb (last input)
            age-numb (age input last-numb)]
        (if (= (count input) 2020)
          last-numb
          (if (pos? age-numb)
            (recur (conj input age-numb))
            (recur (conj input 0)))))))

  )


;; 2nd way : using map

;input [1 2], 3
; output [2 3]
(defn renew [record new]
  (case (count record)
    1 (conj record new)
    2 [(record 1) new]))

(def play {0 [0], 3 [1], 6 [2], :last-numb 6 :last-ind 2})

(defn get-next-numb [play]
  (let [ind (play :last-ind)
        record (play (play :last-numb))]
    (case (count record)
      1 (-> play
            (update 0 renew (inc ind))
            (assoc :last-numb 0 :last-ind (inc ind)))
      2 (let [numb (apply - (reverse record))]
          (-> (if (contains? play numb)
                (update play numb renew (inc ind))
                (assoc play numb [(inc ind)]))
              (assoc :last-numb numb :last-ind (inc ind)))))))

(comment

  (time (loop [play play]
         (if (= (:last-ind play) 2019)
           (:last-numb play)
           (recur (get-next-numb play)))))
  ;->"Elapsed time: 8.741051 msecs", map is much more efficient than vector

  (time (loop [play play]
          (if (= (:last-ind play) (dec 30000000))
            (:last-numb play)
            (do (println play)
              (recur (get-next-numb play)))))))


;; 3rd way : using map (from Fred Overflow)

{0 0, 3 1, 6 2} 0   3
{0 3, 3 1, 6 2} 3   4
{0 3, 3 4, 6 2} 3   5
{0 3, 3 5, 6 2} 1   6
{0 3, 3 5, 6 2, 1 6} 0    7
{0 7, 3 5, 6 2, 1 6} 4    8

(loop [spoken {0 0, 3 1, 6 2}
       numb 0
       ind 3]
  (println ind)
  (if (= ind (dec 30000000))
    numb
    (if (contains? spoken numb)
      (recur (assoc spoken numb ind)
             (- ind (get spoken numb))
             (inc ind))
      (recur (assoc spoken numb ind)
             0
             (inc ind)))))
