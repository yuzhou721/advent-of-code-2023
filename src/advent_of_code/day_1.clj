(ns advent-of-code.day-1
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))


(def input-file-part-1 "day1-1")

(def input-file-part-2 "day1-2")

(defn get-num-line [line]
  (let [nums (re-seq #"\d" line)
        first-str  (first nums)
        last-str  (first (reverse nums))
        num (parse-long (str first-str last-str))]
    ;; (println "nums:" nums)
    ;; (println "first:" first-str)
    ;; (println "last:" last-str)
    ;; (println "num" num)
    num))

(defn get-sum-num [lines]
    (loop [sum 0 seq lines]
      (if (empty? seq)
        sum
        (recur (+ sum (get-num-line (first seq))) (rest seq)))))

(def num-mapper {"one" "1"
                 "two" "2"
                 "three" "3"
                 "four" "4"
                 "five" "5"
                 "six" "6"
                 "seven" "7"
                 "eight" "8"
                 "nine" "9"})

(def num-mapper-2 {"oneight" "18"
                   "twone" "21"
                   "threeight" "38"
                   "fiveight" "58"
                   "sevenine" "79"
                   "eightwo" "82"
                   "eighthree" "83"
                   "nineight" "98"})

(defn replace-digits [str]
  (string/replace str (re-pattern (string/join "|" (keys num-mapper))) num-mapper))

(defn replace-digits-2 [str]
  (string/replace str (re-pattern (string/join "|" (keys num-mapper-2))) num-mapper-2)
  )

(defn part1 []
  (-> input-file-part-1
      util/read-input
      util/->line
      get-sum-num))

(defn part2 []
  (-> input-file-part-2
      util/read-input
      replace-digits-2
      replace-digits
      util/->line
      get-sum-num))
