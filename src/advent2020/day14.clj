(ns advent2020.day14)

(def small "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1")
(def large (slurp "resources/day14.txt"))

(defn parse-memory [memory]
  (->> memory
       (re-seq #"(?:mask = ([01X]+))|(?:mem\[(\d+)\] = (\d+))")
       (map (fn [[_ mask address value]]
              (if (nil? mask)
                {:address (Long/parseLong address) :value (Long/parseLong value)}
                {:and (-> (clojure.string/replace mask #"X" "1")
                          (Long/parseLong 2))
                 :or  (-> (clojure.string/replace mask #"X" "0")
                          (Long/parseLong 2))}
                )))))

(def mem (parse-memory large))

(def result
  (reduce
    (fn [acc {:keys [and or address value]}]
      (if (some? and)
        (assoc acc :and and :or or)
        (assoc acc address (->> value
                                (bit-and (acc :and))
                                (bit-or (acc :or))))))
    {:and 1 :or 0}
    mem))

(comment
  (->> (dissoc result :and :or)
       vals
       (apply +)))                                          ;part 1


(defn parse-memory* [memory]
  (->> memory
       (re-seq #"(?:mask = ([01X]+))|(?:mem\[(\d+)\] = (\d+))")
       (map (fn [[_ mask address value]]
              (if (nil? mask)
                {:address (Long/parseLong address) :value (Long/parseLong value)}
                {:mask mask}
                )))))

(def mem* (parse-memory* large))

(defn to36bit [number]
  (let [numb (Long/toBinaryString number)
        numb-in36 (->> numb
                       (concat (repeat (- 36 (count numb)) \0))
                       (apply str))]
    numb-in36))

;input [mask address]: "000000000000000000000000000000X1001X" 42
;output: "000000000000000000000000000000X1101X"
(defn put-mask [mask address]
  (let [address (to36bit address)]
    (->> (map (fn [m a]
                (case m
                  \0 a
                  \1 \1
                  \X \X))
              mask address)
         (apply str))))

;input "000000000000000000000000000000X1101X" "11"
;output "000000000000000000000000000000111011"
(defn frame [frame numbs]
  {:pre [(= (count (filter #(= % \X) frame)) (count numbs))]}
  (loop [frame frame
         numbs (map str numbs)]
    (if (empty? numbs)
      frame
      (recur (clojure.string/replace-first frame #"X" (first numbs)) (rest numbs)))))

;input 3
;output ("111" "110" "101" "100" "011" "010" "001" "000")
(defn permutation [n]
  (if (= n 1)
    '("1" "0")
    ((fn [coll]
       (concat (map #(str "1" %) coll) (map #(str "0" %) coll))) (permutation (dec n)))))

;input [mask address]: "000000000000000000000000000000X1001X" 42
;output (59 58 27 26)
(defn address-list [mask address]
  (let [masked (put-mask mask address)
        n (count (filter #(= % \X) masked))]
    (->> (permutation n)
         (map #(frame masked %))
         (map #(Long/parseLong % 2)))))

(def result*
  (reduce
    (fn [acc {:keys [mask address value]}]
      (if (some? mask)
        (assoc acc :mask mask)
        (apply assoc acc (interleave (address-list (acc :mask) address) (repeat value)))))
    {:mask ""}
    mem*))

(comment
  (->> (dissoc result* :mask)
      vals
      (apply +)
      ))                                                    ; part2



