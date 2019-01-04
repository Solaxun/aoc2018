(ns aoc2018-clj.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (slurp (io/resource "day2.txt")))
(def data (str/split-lines input))

;; part1 incorrect - see why later (but second version better anyhow)
;; still - figure out where you went wrong to learn from mistakes
(defn checker [freqs]
  (let [freqs (-> freqs vals set)]
    (cond (and (contains? freqs 2) (contains? freqs 3)) :both
          (contains? freqs 2) :two
          (contains? freqs 3) :three
          :else :zero)))

(defn total-up [[two-three two three]]
  (* (+ two-three two) (+ two-three three)))

(->> (map frequencies data)
     (group-by checker)
     (remove (fn [[k v]] (= :zero k)))
     vals
     (map count)
     total-up)

;; part 1 correct
(defn part1 []
  (apply *
         (reduce (fn [[two three] m]
                   (cond (and (contains? m 2) (contains? m 3)) [(inc two) (inc three)]
                         (contains? m 2) [(inc two) three]
                         (contains? m 3) [two (inc three)]
                         :else [two three]))
                 [0 0]
                 (map (comp set vals frequencies) data))))

(part1)

;; part 2
(defn obo? [[s1 s2]]
  (->> (map #(if (= %1 %2) 0 1) s1 s2)
       (apply +)
       (= 1)))

(defn part2 []
  (str/join (apply map #(when (= %1 %2) %1)
                   (first (filter obo? (combs/combinations data 2))))))

(part2)
