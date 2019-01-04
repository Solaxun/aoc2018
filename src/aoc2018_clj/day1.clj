(ns aoc2018-clj.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "day1.txt")))
(def data (map read-string (str/split-lines input)))

(defn part1 []
  (->> data (apply +)))

(part1)

(defn part2 []
  (loop [d (vec data)
         i 0
         seen #{}
         freq 0
         size (count d)]
    (let [cur (d i)
          freq (+ cur freq)]
      (if (seen freq)
        freq
        (recur d (mod (inc i) size) (conj seen freq) freq size)))))

(part2)

(defn nicer-part2 []
  (loop [d (cycle data)
         freq 0
         seen #{}]
    (if (seen freq)
      freq
      (recur (rest d) (+ freq (first d)) (conj seen freq)))))

(nicer-part2)
