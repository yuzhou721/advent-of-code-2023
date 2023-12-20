(ns advent-of-code.day-3
  (:require [advent-of-code.util :as util]
            [clojure.string :as string]))


(defn get-position [str num start]
  (.indexOf str num start))

(defn is-near-symple [str start end]
  (let [inner-start (if (neg? start) 0 start)
        str-length (count str)
        inner-end (if (< str-length end) str-length end)]
    ;; (println "str:" str)
    ;; (println "inner-start:" inner-start)
    ;; (println "inner-end:" inner-end)
    (let [check (re-find #"[^0-9.a-zA-Z]" (subs str inner-start inner-end))]
      (if (empty? check)
        false
        true))))

(defn is-near-* [*-index nums]
  (->> nums
   (map #(let [start (nth % 1)
               end (nth % 2)
               num (first %)]
           (if (and (<= (dec start) *-index) (<= *-index (inc end)))
             num
             nil)))
       (filter some?)))

(defn get-valiable-digit [num-str start current before after]
  (let [index (get-position current num-str start)
        start (dec index)
        end (inc (+ index (count num-str)))
        num (parse-long num-str)]
    (cond (is-near-symple current start end) num
          (is-near-symple before start end) num
          (is-near-symple after start end) num
          :else 0)))

(defn get-digits-index [all-nums str]
    (loop [nums all-nums
           content str
           index 0
           lst []]
      (if (empty? nums)
        lst
        (let [num (first nums)
              len (count num)
              start (string/index-of content num)
              current (+ index start len)
              start-index (+ start index)]
          (recur (rest nums)
                 (subs str current)
                 current
                 (conj lst [(parse-long num) start-index (dec (+ start-index len))]))))))

(defn get-digits [str]
  (get-digits-index (re-seq #"\d+" str) str))

(defn get-char-indexs [str char]
  (loop [content str
         index 0
         lst []]
    (if-not (string/includes? content char)
      lst
      (let [start (string/index-of content char)
            current-lst (conj lst (+ start index))
            current (+ index start 1)]
        (recur (subs str current)
               current
               current-lst)))))


(defn get-data [split-str]
  (let [before (first split-str)
        current (second split-str)
        after (nth split-str 2)
        nums (get-digits current)]
    (if (empty? nums)
      (->> nums
           (map #(get-valiable-digit (first %) (second %) current before after))
           (reduce +))
      0)))

(defn get-data-part2 [split-str]
  (let [
        before (first split-str)
        current (second split-str)
        after (nth split-str 2)
        before-nums (get-digits before)
        current-nums (get-digits current)
        after-nums (get-digits after)
        all-nums (concat before-nums current-nums after-nums)
        star-index (get-char-indexs current "*")]
    (if (or (empty? all-nums) (empty? star-index))
      0
      (let [digits (map #(is-near-* % all-nums) star-index)
            digits (doall digits)]
        (->> digits
             (filter #(= 2 (count %)))
             (map #(* (first %) (second %)))
             (reduce +))))))

(defn partition-pad [len] (string/join (repeat len ".")))

(defn split-near-str [lst]
	(let [len (count (first lst))]
      (conj (partition 3 1 [(partition-pad len)] lst) (list (partition-pad len) (first lst) (second lst)))))

(def example-input "day3-example")

(def file-input "day3-1")

(defn part-1 []
  (-> file-input
      util/read-input
      util/->line
      split-near-str
      (#(map get-data %))
      (#(reduce + %))))

(defn part-2 []
  (-> file-input
      util/read-input
      util/->line
      split-near-str
      (#(map get-data-part2 %))
      (#(reduce + %))))

