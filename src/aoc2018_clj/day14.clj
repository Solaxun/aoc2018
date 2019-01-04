(ns aoc2018-clj.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(defn recipe [n]
  (loop [scores [3 7]
         i 0
         j 1
         turns 0]
    (if (>= (count scores) (+ n 10))
      (subvec scores n (+ n 10))
      (let [[a1 b1] (->> (+ (scores i) (scores j)) str (map (comp read-string str)))
            new-scores (into scores (if b1 [a1 b1] [a1]))]
        ;;(println new-scores a1 b1 i j)
        (recur new-scores
               (mod (+ i (inc (scores i))) (count new-scores))
               (mod (+ j (inc (scores j))) (count new-scores))
               (inc turns))))))

;; part 1
(apply str (recipe 920831))

;; part 2 - needed heavy optmization inspired by reddit
(defn recipe2 [n]
  (let [vecn (map (comp read-string str) (str n))
        cnt (count vecn)]
    (loop [scores [3 7]
           i 0
           j 1
           c 2]
      (cond
        (= (subvec scores (max 0 (- c cnt)) c) vecn)
        (count (drop-last cnt scores))

        (= (subvec scores (max 0 (- c cnt 1)) (dec c)) vecn)
        (count (drop-last (inc cnt) scores))

        :else
        (let [s (+ (scores i) (scores j))
              tens (quot s 10)
              remn (mod  s 10)
              new-scores (if (not= tens 0)
                           (conj scores tens remn)
                           (conj scores remn))]
          (recur new-scores
                 (mod (+ i (inc (scores i))) (+ c (inc tens)))
                 (mod (+ j (inc (scores j))) (+ c (inc tens)))
                 (+ c (inc tens))))))))

(recipe2 920831)
