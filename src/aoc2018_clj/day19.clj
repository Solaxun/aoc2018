(ns aoc2018-clj.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day19.txt"))))

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
      {opname #(i %1 %2 (fname->func op))} ; had r and i flipped... huge bug
      {opname #(r %1 %2 (fname->func op))})))

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

(def instructions
  (vec (mapcat (fn [[ins & nums]]
                 [(vec (concat [ins] (map read-string nums)))])
               (map rest (re-seq #"(\w+) (\d+) (\d+) (\d+)"
                                 input)))))

;; was VERY confused for two hours... the register is bound to pointer once at
;; outset, but the VALUE of that same pointer changes even tho the register it
;; references will not.
(defn execute [instructions regs pointer]
  (loop [ip 0
         regs regs]
    ;;(println "before" regs ip)
    (if (or (>= ip (count instructions)) (< ip 0))
      regs
      (let [instr (instructions ip)
            [opcode a b c] instr
            new-reg ((get all-funcs opcode)
                     instr
                     (assoc regs pointer ip)) ; set new pointer in reg
            new-ip (inc (new-reg pointer))]
        ;;(println "after" new-reg new-ip)
        (recur new-ip
               new-reg)))))

;; part 1
(execute instructions [0 0 0 0 0 0] 2)
;; part 2 - will run for long time, figure out loop manually
#_(execute instructions [1 0 0 0 0 0] 2)
