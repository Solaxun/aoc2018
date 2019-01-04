(ns aoc2018-clj.day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day18.txt"))))
(def data (mapv vec (str/split-lines input)))

(defn neighbors8 [[x y]]
  (for [delta-x [-1 0 1]
        delta-y [-1 0 1]
        :when (not= [delta-x delta-y] [0 0])]
    (mapv + [x y] [delta-x delta-y])))

(defn oob? [[x y]]
  (or (< x 0) (> x 49) (< y 0) (> y 49)))

(defn adjacent-trees [locs] (count (filter #(= % \|) locs)))
(defn adjacent-lumber [locs] (count (filter #(= % \#) locs)))
(defn lumber-stay [locs] (and (>= (adjacent-lumber locs) 1)
                              (>= (adjacent-trees locs) 1)))

(defn tick [lumber-yard]
  (for [x (range (count lumber-yard))
        y (range (count lumber-yard))
         :let [neighbors (map #(get-in lumber-yard %) (remove oob? (neighbors8 [x y])))
              item (get-in lumber-yard [x y])]]
    (case item
      \# (if (lumber-stay neighbors) \# \.)
      \. (if (>= (adjacent-trees neighbors) 3) \| \.)
      \| (if (>= (adjacent-lumber neighbors) 3) \# \|)
      item)))

(defn do-tick [data] (mapv vec (partition (count data) (tick data))))
(def gen-10 (nth  (iterate do-tick data) 10))

(defn resource-value [lumber-yard]
  (* (count (filter  #(= % \|) (flatten lumber-yard)))
     (count (filter  #(= % \#) (flatten lumber-yard)))))

(defn part1 []
  (resource-value (nth (iterate do-tick data) 10)))

(part1)

;; part 2 - eyeballing shows starts repeating with 140112 at some point
(map resource-value (take 100 (iterate do-tick data)))
;; 409 generations occur before the cycle starts
(count (take-while #(not= 140112 (resource-value %)) (rest (iterate do-tick data))))
;; manually extracted cycle from eyeballing output
(def -cycle [140112 141502 139590 140976 146070 152900 160720 169312 181792 186319 195936 206700 218491 223011 228683 226416 224082 222856 225822 224455 218736 209024 195250 185265 177331 167334 151965 145632 136896 138325 136948 139896 143648 150700 157348 167321 176712 181503 188490 199584 209780 214446 217801 214099 213324 213408 216445 212646 209190 199245 189350 178416 171312 162972 155647 145912 135516 135904 134034 135182 139916 146882 154284 162418 174042 179376 188496 199694 212647 217752 223168 219998 221760 221520 224721 221260 216293 205821 195408 184481 178653 170340 158316 148500])

;; would expect this to work with 409, not 410 - as 409 elements had been seen
;; before cycle starts, not 410 (excluding the 0th generation prior to any runs)
;; just tried to move left or right one to test OBO and got lucky - not sure why
;; it's not 409 still
(-cycle (mod (- 1000000000 410) (count -cycle)))
