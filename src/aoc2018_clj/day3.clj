(ns aoc2018-clj.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day3.txt")))
(def data (str/split-lines input))

(defn make-square [[col row col-height row-height]]
  [[col row]
   [(+ col (dec col-height)) (+ row (dec row-height))]])

(defn parse-id [id]
  (let [[match id col row col-sz row-sz]
        (re-find #"(\d+) @ (\d+),(\d+): (\d+)x(\d+)" id)]
    (map read-string [col row col-sz row-sz])))

(defn overlapping? [square1 square2]
  (let [[[x1 y1] [x2 y2]] square1
        [[x3 y3] [x4 y4]] square2]
    [[(- x3 x1) (- x4 x2)]
     [(- y3 y1) (- y4 y2)]]))

(defn points-between [[[x1 y1] [x2 y2]]]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    [x y]))

  (def test-data
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(defn all-points [data]
  (->> (map parse-id data)
       (map make-square)
       (mapcat points-between)))

(defn part-1 [data]
  (println (clojure.set/intersection (set )))
  (- (count (all-points data))
     (count (set (all-points data)))))

(part-1 data)
;; (points-between (make-square (parse-id (first test-data))))
;; (count (points-between (make-square (parse-id "#123 @ 338,764: 20x24"))))
;; (clojure.set/intersection (mapcat points-between (map make-square (map parse-id test-data))))

(clojure.set/union
 #{[1 2] [3 4]}
 #{[6 4] [6 6]}
 #{[6 4] [6 5]})

(count (filter (fn [[pt ct]] (>= ct 2))
               (frequencies (all-points data))))

(defn all-squares [data]
  (->> (map parse-id data)
       (map make-square)))
(def freqs (frequencies (all-points data)))

(filter (fn [square-points]
       (every? #(= 1 (freqs %)) square-points))
     (map points-between (all-squares data)))
