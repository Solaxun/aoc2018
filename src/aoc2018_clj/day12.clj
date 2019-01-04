(ns aoc2018-clj.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def transitions (-> "day12.txt" io/resource slurp))
(def prev->next  (into {} (map #(str/split % #" => ") (str/split-lines transitions))))
(def start (str "#...#..###.#.###.####.####.#..#.##..#..##..#....."
                "#.#.#.##.#...###.#..##..#.##..###..#..##.#..##..."))

(defn grow [start]
  (->> (str "...." start "....")
       (partition 5 1)
       (map str/join)
       (map #(get prev->next % "."))
       str/join))

;; part 1
(defn part1 [n]
  (as-> (iterate grow start) xs
    (nth xs n)
    (map vector (range (* -2 n) (- (count xs) (* 2 n))) xs)
    (keep (fn [[ix plant]] (when (= plant \#) ix)) xs)
    (reduce + xs)))

(part1 20)

;; part 2 - printing shows stabilization at iteration 94
;;(map-indexed #(println %1 %2) (take 100 (iterate print-friendly-grow start))) ;stabilizes
(defn print-friendly-grow [start]
  (->> (str "...." start "....")
       (partition 5 1)
       (map str/join)
       (map #(get prev->next % "."))
       (drop-while #(= % "."))
       (reverse)
       (drop-while #(= % "."))
       (reverse)
       str/join))

(defn get-ix [n]
  (as-> (iterate grow start) xs
    (nth xs n)
    (map vector (range (* -2 n) (- (count xs) (* 2 n))) xs)
    (keep (fn [[ix plant]] (when (= plant \#) ix)) xs)))

(let [n (- 50000000000 94)]
  (reduce + (map (partial + n) (get-ix 94))))
