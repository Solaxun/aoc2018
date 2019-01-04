(ns aoc2018-clj.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(defn hundreds-digit [n]
  (let [sn (str n)]
   (if (< (count (take-last 3 sn)) 3)
     0
     (-> (first (take-last 3 sn)) str read-string))))

(defn cell-power [serial-number [x y]]
  (-> (+ x 10)
      (* y)
      (+ serial-number)
      (* (+ x 10))
      hundreds-digit
      (- 5)))

(defn total-cell-power [serial-number square]
  (reduce + (map (fn [points]
    (reduce + (map (partial cell-power serial-number)
            points)))
  square)))

(def coords (vec (for [x (range 300) y (range 300) ] [x y])))

(defn slice-and-dice [square-size coords]
  (let [row-size (Math/sqrt (count coords))]
    (for [i (range (inc (- (- (count coords)
                              (* (dec square-size) row-size))
                           square-size)))]
      (for [j (range square-size)]
        (subvec coords
                (+ i (* j row-size))
                (+ i (* j row-size) square-size))))))

;; lazy hack because i can't figure out how to properly
;; build 3x3 subgrids without filtering bad coords after
(defn square-in-bounds? [square]
 (every? #(apply = %)
   (map (fn [v] (map first v)) square)))

;; part 1
(defn part1 []
  (let [board (filter square-in-bounds? (slice-and-dice 3 coords))]
    (ffirst (apply max-key (partial total-cell-power 6303) board))))

;;part 2
(def winner
  (apply max-key (partial total-cell-power 6303)
         (map #(apply max-key (partial total-cell-power 6303) %)
              (for [i (range 1 301)]
                (filter square-in-bounds? (slice-and-dice i coords))))))

((juxt first count) winner)

(apply max-key (partial total-cell-power 6303)
       (filter square-in-bounds? (slice-and-dice 40 coords)))
