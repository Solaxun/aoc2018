(ns aoc2018-clj.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combs]))

(def input (str/trim (slurp (io/resource "day4.txt"))))

(def guard-info (str/split-lines input #_guard-info))

(defn parse-time [time]
 (.parse (java.text.SimpleDateFormat. "y-M-d k:m") time))

(defn sort-observations [observations]
  (sort-by (fn [observation]
            (-> (re-find #"\[(.*)\]" observation)
                (last)
                (parse-time)))
           observations))

(defn parse-observation [observation]
  (let [[match time event num]
        (re-find #"\[(.*)\] (Guard|falls|wakes) #?(\d+)?" observation)]
      [(parse-time time) event num]))

(defn jdate->ymd [jdate]
 (let [dt (java.time.LocalDateTime/ofInstant
          (.toInstant  jdate)
          (java.time.ZoneOffset/UTC))
       y (.getYear dt)
       m (.getMonthValue dt)
       d (.getDayOfMonth dt)
       h (.getHour dt)
       mn (.getMinute dt)]
    [dt y m d h mn]))

(defn guard->statuses [guard-times]
  (loop [obs (map parse-observation (sort-observations guard-times))
         guard-num nil
         guards {}]
      (if (seq obs)
        (let [[time event num] (first obs)
              [dt y m d h mn] (jdate->ymd time)]
          (recur (rest obs)
                 (if num num guard-num)
                 (if (= event "Guard")
                      (update guards num (fnil conj []) [mn "wakes"])
                      (update guards guard-num (fnil conj []) [mn event]))))
          guards)))

(defn build-intervals [status-vec]
  (reduce (fn [[m [time1 status1]] [time2 status2]]
    ;; new shift - accumulate range till 59 from prior status
    (if (< time2 time1)
        [(assoc m (range time1 60) status1 (range 0 time2) status1) [time2 status2]]
        ;; existing shift - accumulate range till t2 from prior status
        [(assoc m (range time1 time2) status1) [time2 status2]]))
   [{} (first status-vec)]
   (concat (drop 1 status-vec)
           (conj status-vec [60 "dont-matter"]))))

(def guard->intervals
  (->> guard-info
       guard->statuses
       (map (fn [[g ivs]] {g (-> ivs build-intervals first)}))
       (apply merge)))

(defn sleepiest [guard->intervals]
  (reduce-kv (fn [m g statuses]
               (assoc m g
                      (reduce (fn [total [iv status]]
                                (if (= status "falls") (+ total (count iv)) total))
                              0
                              statuses)))
             {}
             guard->intervals))

(def slept-time
  (apply max-key val (sleepiest guard->intervals)))

(def most-time (second slept-time))
(def sleepiest-worker (first slept-time))

(->> (guard->intervals sleepiest-worker)
     (filter (fn [[k v]] (= "falls" v)))
     (map first)
     (apply concat)
     frequencies
     (apply max-key second)
     first
     (* (read-string sleepiest-worker)))

;; part 2
(def guard->sleepfreq
  (for [[guard intervals] guard->intervals]
    [guard
     (frequencies
      (apply concat
             (keep (fn [[iv status]] (when (= status "falls") iv))
                   intervals)))]))
(reduce
 (fn [[maxg maxmin maxfreq] [guard freq]]
   (let [[curmin curfreq] (apply max-key second freq)]
     (if (> curfreq maxfreq)
       [guard curmin curfreq]
       [maxg maxmin maxfreq])))
 [nil 0 0]
 (remove (fn [[guard freq]] (empty? freq)) guard->sleepfreq))
(* 2999 24)
