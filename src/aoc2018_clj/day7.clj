(ns aoc2018-clj.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]
            [clojure.set :as set]))

(def input (str/trim (slurp (io/resource "day7.txt"))))

#_(def input
  "Step C must be finished before step A can begin.
  Step C must be finished before step F can begin.
  Step A must be finished before step B can begin.
  Step A must be finished before step D can begin.
  Step B must be finished before step E can begin.
  Step D must be finished before step E can begin.
  Step F must be finished before step E can begin.")
(def pairs (partition 2 (map str/trim (re-seq #"\s[A-Z]\s" input))))

(def before->after
  (reduce (fn [m [before after]]
            (if (contains? m before)
              (assoc m before (conj (m before) after))
              (assoc m before [after])))
          {}
          pairs))

(def after->before
  (reduce (fn [m [before after]]
            (if (contains? m before)
              (assoc m before (conj (m before) after))
              (assoc m before [after])))
          {}
          (for [[before after] before->after
                node after]
            [node before])))

(def start
  (vec (reverse (sort (clojure.set/difference
                       (set (keys before->after))
                       (set (keys after->before)))))))

(defn part1 []
  (loop [candidates start
         visited #{}
         ordered []]
    (let [current (peek candidates)]
      ;; (println candidates "->" visited"->" ordered)
      (cond (empty? candidates)
            (str/join ordered)
            (and (not (contains? visited current))
                 ;; node's dependencies have been met
                 (empty? (remove visited (after->before current))))
            (recur (vec (reverse (remove visited (sort (concat (pop candidates) (before->after current))))))
                   (conj visited current)
                   (conj ordered current))
            :else
            (recur (pop candidates)
                   visited
                   ordered)))))
(part1)

(defn node->time [node]
  (get (zipmap
        (map char (range 97 (+ 97 26)))
        (iterate inc 1))
       node))

(defn prereqs-met? [[node time status :as current] completed]
  (empty? (remove (set completed) (after->before node))))

(def node->time (zipmap (map (comp str char) (range 65 (+ 65 26))) (range (+ 60 1) (+ 60 27))))

(defn build-node [node]
  (vector node (node->time node) :pending))

(defn get-neighbors [node]
  (->> node
       before->after
       (map build-node)))

(defn remove-from [c1 c2]
  (remove (fn [[node cnt status]] (contains? (set (map first c2)) node))
          c1))

(defn distribute-work [n candidates]
"assign work to those that are available alphabetically
then decrement all those that are working"
  (let [working     (filter #(= (get % 2) :working) candidates)
        capacity    (- n (count working ))
        blocked     (filter #(= (get % 2) :blocked) candidates)
        choices     (sort-by first (filter #( = (get % 2) :pending) candidates))
        new-workers (take capacity choices)
        pending     (remove-from (filter #(= (get % 2) :pending) candidates) new-workers)]
    (concat (map (fn [node] (update node 1 dec)) working)
            (map (fn [node] (-> node (update 1 dec) (assoc 2 :working))) new-workers)
            pending
            blocked)))

(loop [working (mapv #(assoc % 2 :working) (map build-node start)) ;[node cnt satus]
       ordered []
       i 0]
       (println working)
 (if (or (empty? working) (> i 500))
   [i (apply str ordered) (count ordered)]
   ;; work complete can be blocked by dependencies or ready to exp
   (let [completed  (filterv (comp zero? second) working)
         ;;finished + deps met
         expandable (filterv #(prereqs-met? % ordered) completed)
         ;;finished deps not met
         blocked    (map #(assoc % 2 :blocked) (filter #(not (prereqs-met? % ordered)) completed))
         neighbors  (mapcat (comp get-neighbors first) expandable)
         ;; dont just remove if visited (ordered), also if on queue already (working)
         neighbors  (remove (fn [[node cnt status]] (contains? (set (map first working)) node)) neighbors)
         still-working   (filter (fn [[n t status]] (and (pos? t) (= status :working))) working)
         pending   (filter #(= (get % 2) :pending) working)]
     (recur (distribute-work 5 (concat still-working pending blocked neighbors))
            (into ordered (map first expandable))
            (inc i)))))
;;(part2)
;; working or pending or blocked -> pending depends on working count and capacity
;; blocked depends on prereqs
#_(defn curry [f n]
  (fn c [& args] (if (>= (count args) n)
                (apply f args)
                (fn [b] (apply c (concat args [b])))) ))
(node->time "Z")
(after->before "E")
(before->after "E")
