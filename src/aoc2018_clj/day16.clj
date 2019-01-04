(ns aoc2018-clj.day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day16.txt"))))
(def program-samples (first (str/split input #"\n\n\n")))
(def program
  (->> (str/split input #"\n\n\n")
       last
       (re-seq #"\d+")
       (map read-string)
       (partition 4)))

(def program-samples
  (->> program-samples
       (re-seq #"\d+" )
       (map read-string)
       (partition 4)
       (partition 3)))

(def fname->func
  {"add" +
   "mul" *
   "ban" bit-and
   "bor" bit-or
   "gt"  >
   "eq"  =})

(defn r [[opcode a b c] reg f] (assoc reg c (f (reg a) (reg b))))
(defn i [[opcode a b c] reg f] (assoc reg c (f (reg a) b)))
(defn setr [[opcode a b c] reg f] (f reg c (reg a)))
(defn seti [[opcode a b c] reg f] (f reg c a))
(defn ir [[opcode a b c] reg op] (if (op a (reg b)) (assoc reg c 1) (assoc reg c 0)))
(defn ri [[opcode a b c] reg op] (if (op (reg a) b) (assoc reg c 1) (assoc reg c 0)))
(defn rr [[opcode a b c] reg op] (if (op (reg a) (reg b)) (assoc reg c 1) (assoc reg c 0)))

(def ops
  (for [op ["add" "mul" "ban" "bor"]
        suffix ["i" "r"]
        :let [opname (str op suffix)]]
    (if (= suffix "i")
      {opname #(r %1 %2 (fname->func op))}
      {opname #(i %1 %2 (fname->func op))})))

(def cmps
  (for [cmp ["gt" "eq"]
        suffix ["ir" "ri" "rr"]
        :let [cmpname (str cmp suffix)]]
    (cond (= suffix "ir") {cmpname #(ir %1 %2 (fname->func cmp))}
          (= suffix "ri") {cmpname #(ri %1 %2 (fname->func cmp))}
          (= suffix "rr") {cmpname #(rr %1 %2 (fname->func cmp))})))

(def all-funcs
  (apply merge {"seti" #(seti %1 %2 assoc) "setr" #(setr %1 %2 assoc)}
         (concat ops cmps)))
;; part 1
(defn part1 [& {n :n :or {n 1}}]
  (count
   (for [[regs instr after] program-samples
         :let [apply-all (filter (fn [[fname f]] (= after (f instr (vec regs)))) all-funcs)]
         :when (>= (count apply-all) n)]
     regs)))

(part1 :n 3)

;; part 2
(def opcode->funcname
  {0 "eqir" 1 "bori" 2 "addi" 3 "gtri" 4 "mulr"
   5 "gtir" 6 "muli" 7 "bani" 8 "borr" 9 "eqri"
   10 "eqrr" 11 "banr" 12 "setr" 13 "gtrr" 14 "addr" 15
   "seti" })

(defn run-program [instructions]
  (reduce (fn [regs [opcode a b c :as instr]]
            ((-> opcode opcode->funcname all-funcs) instr regs))
          [0 0 0 0]
          instructions))

(defn part2 []
  (run-program program))

(part2)

(loop [[[regs instr after] & programs] program-samples
       opcode->funcname {}]
  (filter (fn [[fname f]] (= (f instr (vec regs)) after)) all-funcs))
