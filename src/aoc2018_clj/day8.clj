(ns aoc2018-clj.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day8.txt"))))
(def nums (mapv read-string (re-seq #"\d+" input)))

#_(def nums [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn process [nums]
  (loop [i 2
         metadata []
         pending [(subvec nums 0 2)]]
    ;;(println i metadata pending)
    (if (>= i (count nums))
      [metadata pending]
      (let [[prevc prevm] (peek pending)
            [c m] (subvec nums i (+ 2 i))]
        (cond
          ;; current child empty
          (zero? c)
          (recur (+ i 2 m)
                 (into metadata (subvec nums (+ i 2) (+ i 2 m)))
                 (conj (pop pending) [(dec prevc) prevm]))
          ;; parent empty
          (zero? prevc)
          (recur (+ i prevm)
                 (into metadata (subvec nums i (+ i prevm)))
                 (if (> (count pending) 1)
                   (update-in (pop pending) [(- (count pending) 2) 0] dec)
                   (pop pending)))
          ;; children to add
          :else
          (recur (+ i 2)
                 metadata
                 (conj pending (subvec nums i (+ i 2)))))))))

(let [[m p] (process nums)]
  (println "stack->" p "\n" "meta->\n"m "sum->"(reduce + m)))
