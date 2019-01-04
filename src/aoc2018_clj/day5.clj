(ns aoc2018-clj.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day5.txt"))))

(defn lower-case? [letter] (= (str letter) (str/lower-case letter)))
(defn upper-case? [letter] (= (str letter) (str/upper-case letter)))

(defn polarized? [[a b]]
  (cond (lower-case? a) (= (str/upper-case a) (str b))
        (upper-case? a) (= (str/lower-case a) (str b))
        (lower-case? b) (= (str/upper-case b) (str a))
        (upper-case? b) (= (str/lower-case b) (str a))
        :else false))

;; part 1
(defn collapse [polymer]
  (loop [s polymer
         ix 0]
    (cond (> ix (- (count s) 2)) (count s)
          (polarized? (subs s ix (+ 2 ix))) (recur (str (subs s 0 ix)
                                                        (subs s (+ 2 ix)))
                                                   (max 0 (dec ix)))
          :else (recur s (inc ix)))))
(collapse input)

;; part 2
(apply min (map (fn [[a b]]
            (-> input
                (str/replace (str a) "")
                (str/replace (str b) "")
                collapse))
          (partition 2 (set input))))
