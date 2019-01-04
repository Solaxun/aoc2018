(ns aoc2018-clj.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]
            [clojure.set :as set]))

(def track (str/split-lines (-> "day13.txt" io/resource slurp)))
(def race-track (mapv vec track))

(defn intersection-rule [{:keys [loc dir turn-count] :as car}]
  (assoc car :dir
   (case (mod turn-count 3)
     0  ({\> \^ \< \v \v \> \^ \<} dir) ;; turn left
     1  dir                             ;; straight
     2 ({\> \v \< \^ \v \< \^ \>} dir)) ;; turn right
   :turn-count (inc turn-count)))

(defn change-dir
  [{:keys [loc dir turn-count] :as car} road-segment]
  (case road-segment
   \/ (update car :dir {\^ \> \v \< \> \^ \< \v})
   \\ (update car :dir {\^ \< \v \> \> \v \< \^})
   \+ (intersection-rule car)
   car))

(defn drive [{:keys [loc dir turn-count] :as car}]
 (let [from->to {\> [0 1] \< [0 -1] \^ [-1 0] \v [1 0]}
       destination (mapv + (from->to dir) loc)
       road-segment (get-in race-track destination)]
   (-> (change-dir car road-segment)
       (assoc :loc destination))))

(defn make-cars [initial-track]
  (for [i (range (count initial-track))
        j (range (count initial-track))
        :let [dir (get-in initial-track [i j])]
        :when (contains? #{\< \> \^ \v} dir)]
    {:loc [i j] :dir dir :turn-count 0}))

(defn advance-cars [cars]
 (let [cars (sort-by :loc cars)]
  (reduce
   (fn [[full? new-cars] car]
    (let [next-car (drive car)
          next-loc (:loc next-car)]
      (if (full? next-loc)
        (reduced [#{} [(assoc next-car :collision true)]])
        [(conj (set/difference full? #{(:loc car)}) next-loc)
         (conj new-cars next-car)])))
   [(set (map :loc cars)) []]
   (sort-by :loc cars))))

(defn collision? [cars]
  (some #(when (contains? % :collision) %) cars))

(defn part1 []
  (loop [[full? cars] [#{} (make-cars race-track)]]
    (if-let [collision (collision? cars)]
      collision
      (recur (advance-cars cars)))))

;;pt 2
(defn advance-cars2 [cars]
  (loop [[c & cs] (sort-by :loc cars)
         full? (set (map :loc cars))
         new-cars []]
    (if (empty? c)
      new-cars
      (let [new-car (drive c)
            new-loc (:loc new-car)]
        (if (full? new-loc)
          (recur (remove #(= (:loc %) new-loc) cs) ;remove preserves sorted orer
                 (disj full? (:loc c) new-loc)
                 ;; collision might be in previously moved cars from this tick
                 (remove #(= (:loc %) new-loc) new-cars))
          (recur cs
                 (conj (disj full? (:loc c)) new-loc)
                 (conj new-cars new-car)))))))
(defn part2 []
  (some #(when (= 1 (count %)) %)
        (iterate advance-cars2 (make-cars race-track))))
(part1)
(part2)
