(ns aoc2018-clj.day17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]
            [lanterna.terminal :as t]))

(def input (str/trim (slurp (io/resource "day17.txt"))))
#_(def data (str/split-lines input))

#_(def input "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504")

(def text (str/split-lines input))

(defn build-ranges [coord-text]
  (let [[x y] (-> coord-text (str/split #", ") (#(sort-by first %)))
        x (map read-string (re-seq #"\d+" x))
        y (map read-string (re-seq #"\d+" y))]
    (for [y (range (first y) (inc (or (second y) (first y))))
          x (range (first x) (inc (or (second x) (first x))))]
          [y x])))

(def clay-locs (mapcat build-ranges text))
(def bounds (apply map (juxt min max) clay-locs))

(def board
  (let [[[ymin ymax] [xmin xmax]] bounds]
    (vec (repeat (inc (- ymax ymin)) (vec (repeat (+ 2 (- xmax xmin))  "."))))))

(defn fill-board [board item coords]
 (reduce #(assoc-in %1 %2 item) board coords))

 (defn coords->zero-based [[[miny maxy][minx maxx] :as bounds] [y x]]
   [(max 0 (- y miny)) (max 0 (- x minx))])

(def bounded-board
 (->> clay-locs
     (map (partial coords->zero-based bounds))
     ((partial fill-board board "#"))))

(def water-loc (coords->zero-based bounds [1 500])) ; manual fix 0 -> 1 bc zero basing is neg at 0

(defn print-board [board]
  (doseq [ln (assoc-in board water-loc "+")]
   (println (str/join ln))))

(defn neighbors-4 [loc]
  (mapv #(mapv + loc %) [[0 -1] [0 1] [1 0] [-1 0]]))

(defn oob?
 ([[y x]] (oob? bounds [y x]))
 ([[[miny maxy] [minx maxx] :as bounds] [y x]]
  (or (< y 0) (> y (- maxy miny)))))

(defn move [dir loc]
  (mapv + loc (case dir
                :down  [1 0]
                :up    [-1 0]
                :left  [0 -1]
                :right [0 1])))

(defn fill-to-side [dir loc blocked?]
  "keep going left until either hits blocked loc or spot below is open"
  (some  #(when (or (blocked? %) ((complement blocked?) (move :down %))) %)
         (iterate (fn [loc] ((partial move dir) loc)) loc)))

(defn xcoords-between [[y1 x1] [y2 x2]]
  "all [y x] coords between a pair of coords on the x axis (y constant)"
  (mapv vector (repeat y1) (range (inc x1) x2)))

(defn pour [water-loc board]
  (loop [water-sources [water-loc]
         blocked? (set (map (partial coords->zero-based bounds) clay-locs))
         board board]
    (let [water (peek water-sources)
          next-spot (move :down water)]
      (cond (or (empty? water-sources))
            (do (print-board board) board)
            ;; pop stack (last water added is pre oob)
            ;; and continue if oob bc waiting for stack to be emptied
            (or (oob? next-spot) (= (get-in board next-spot) "="))
            (do #_(println "oob or =" water-sources water)
                (recur (pop water-sources) blocked? board))
            ;; move l/r from most recent water loc
            (blocked? next-spot)
            (let [rmax (fill-to-side :right  water blocked?)
                  lmax  (fill-to-side :left  water blocked?)
                  new-water-sources (filter #(not (blocked? (move :down %))) [lmax rmax])
                  newly-filled (xcoords-between lmax rmax)
                  #_foo #_(println rmax lmax water next-spot "new" newly-filled "w" water-sources)]
              (recur (into (pop water-sources) (if (seq new-water-sources) ; only move up water
                                                 new-water-sources         ; if no new source
                                                 [(move :up water)]))
                     (into blocked? newly-filled)
                     (if (seq new-water-sources)
                       (fill-board board "=" newly-filled)
                       (fill-board board "~" newly-filled))))

            :else
            (recur (conj (pop water-sources) next-spot)
                   blocked?
                   (-> board (assoc-in next-spot "|")  (assoc-in water "|")))))))

;; 27,331
(->> (pour water-loc bounded-board)
     flatten
     (filter #{"=" "|" "~"})
     count)

#_(->> (pour water-loc bounded-board)
     flatten
     (filter #{"~"})
     count)
