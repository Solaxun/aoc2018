(ns aoc2018-clj.day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/split-lines (str/trim (slurp (io/resource "day6.txt")))))

(def test-input
  "1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9")

(def test-input (str/split-lines test-input))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn make-coords [input]
  (map (comp (partial map read-string) #(str/split % #", ")) input))

(def coords (make-coords input))

(defn bounded? [[x y]]
  (and (some (fn [[x1 y1]] (> x1 x)) coords)
       (some (fn [[x1 y1]] (> y1 y)) coords)
       (some (fn [[x1 y1]] (< x1 x)) coords)
       (some (fn [[x1 y1]] (< y1 y)) coords)))

(def max-x (first (apply max-key first coords)))
(def max-y (second (apply max-key first coords)))
(def infinite-points (set (filter (complement bounded?) coords)))

(defn closest-point [[x y] coords]
  (let [[dist points] (first (sort-by first
                                      (group-by
                                       (partial manhattan-distance [x y])
                                       coords)))]
    (if (> (count points) 1)
           :tie
           (first points))))

(apply max-key second
       (frequencies
        (for [x (range (inc max-x))
              y (range (inc max-y))
              :let [closest (closest-point [x y] coords)]
              :when (and (not= closest :tie)
                         (not (contains? infinite-points closest)))]
          closest)))

infinite-points
(filter bounded? coords)
;;  7434 wrong (71 58)
