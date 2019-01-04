(ns aoc2018-clj.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day10.txt"))))

(def data (partition 4 (map read-string (re-seq  #"-?\d+" input))))

(defn generation []
  (iterate #(mapv (fn [[x y vx vy]]
                    [(+ x vx) (+ y vy) vx vy])
                  %)
           data))

(defn bounds [points]
  (let [[[min-x max-x] [min-y max-y]]
        (apply map (juxt #(apply min %&) #(apply max %&)) points)]
    [min-x max-x min-y max-y]))

(defn make-board [points]
  (let [[minx maxx miny maxy] (bounds points)
        xrange (inc (- maxx minx))
        yrange (inc (- maxy miny))
        empty-board (vec (repeat yrange (vec (repeat xrange "."))))]
    (reduce (fn [b [x y vx vy]]
              (assoc-in b [(+ y (- 0 miny)) (+ x (- 0 minx))] "#"))
            empty-board
            points)))

(defn display-board [board]
  (doseq [b board]
    (println (str/join b))))

(defn max-height [points]
  (let [xcoords (map second points)]
    (- (apply max xcoords) (apply min xcoords))))

;; part 1
(->> (generation)
     (filter #(<= (max-height %) 10))
     first
     make-board
     display-board)

;; pt 2 - includes the initial (pre velocity adjustments) generation
;; so would be one over, but doesn't include the final generation with
;; height that is < 10
(count (take-while #(>= (max-height %) 10) (generation)))
