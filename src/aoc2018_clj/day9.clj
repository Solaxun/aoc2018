(ns aoc2018-clj.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(defn play-marbles [nplayers last-marble]
  (loop [ix 0
         current-player 1
         marbles [0]
         scores {}
         m (inc (marbles ix))]
    (let [new-ix (inc (mod (inc ix) (count marbles)))]
      ;;(println marbles m new-ix)
      (cond (> m last-marble) scores
            (zero? (mod m 23))  (let [remove-ix (mod (- ix 7) (count marbles))]
                                  (recur
                                   (mod (- ix 7) (count marbles))
                                   (mod (inc current-player) nplayers)
                                   (vec (concat (subvec marbles 0 remove-ix)
                                                (subvec marbles (inc remove-ix))))
                                   (update scores
                                           current-player
                                           (fnil + 0)
                                           (+ m (marbles remove-ix)))
                                   (inc m)))

            :else (recur new-ix
                         (mod (inc current-player) nplayers)
                         (vec (concat (subvec marbles 0 new-ix)
                                      [m]
                                      (subvec marbles new-ix)))
                         scores
                         (inc m))))))

;; part 1 ~7 min
(last (apply max-key second (play-marbles 458 71307)))

;; part 2 - not gonna work without diff algorithm
(last (apply max-key second (play-marbles 458 (* 100 71307))))
