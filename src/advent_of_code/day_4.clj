(ns advent-of-code.day-4
  (:require [advent-of-code.util :as util]
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math]))

(def example-input "day_4_example")

(defn parse-nums [str]
  (re-seq #"\d+" str))

(defn parse-round [str]
  (println "rounds-str" str)
  (-> str
      (s/split #"|")
      (#(map parse-nums %))))

(defn parse-line [str]
  (let [[card content] (s/split str #":")
        card (re-find #"\d+" card)
        nums (s/split content #"\|")
        winner-nums (re-seq #"\d+" (first nums))
        wait-nums (re-seq #"\d+" (second nums))
        result (->> wait-nums
             (filter (set winner-nums))
             count)]
    {:card-num card :result-count result}
    )
  )

(defn calculate-part1 [card-result]
  (let [result-count (:result-count card-result)]
    (if (zero? result-count)
      0
      (math/expt 2 (dec result-count)))))

(def file-input "day_4")

(defn part1 []
  (->> file-input
       util/read-input
       util/->line
       (map parse-line)
       (map calculate-part1)
       (reduce +)))

(defn get-cards [card num]
  (let [{card-num :card-num result-count :result-count} card
        card-num (parse-long card-num)]
	(if (zero? result-count)
      nil
      (flatten
       (for [_ (range num)]
                 (range (inc card-num)
                        (+ result-count card-num 1)))))))

(defn calculate-part2 [card-results]
  (loop [cards card-results card-nums (reduce #(assoc %1 (keyword (:card-num %2)) 1) {} card-results)]
    ;; (println "card-nums:" card-nums)
      (if  (empty? cards)
        card-nums
        (let [card (first cards)
              num ((keyword (str (:card-num card))) card-nums)
              new-cards (get-cards card num)]
          (recur (rest cards)
                 (if (some? get-cards)
                   (reduce #(update %1 (keyword (str %2)) inc) card-nums new-cards)
                   card-nums))
          ))
      ))

(defn part2 []
  (->> file-input
       util/read-input
       util/->line
       (map parse-line)
       calculate-part2
       vals
       (reduce +)))
