(ns advent-of-code.day-2
  (:require [clojure.string :as string]
            [advent-of-code.util :as util]))


(def limit {:red 12 :blue 14 :green 13})

(defn find-color [str]
  (let [str-split (string/split (string/trim str) #" ")
        color (second str-split)
        num (first str-split)]
    {(keyword color) (parse-long num)}))

(defn find-step [str]
  (let [str-split (string/split (string/trim str) #",")]
    (reduce merge (map find-color str-split))))

(defn sum-color [one two]
  (merge-with + one two))

(defn check [game]
  (->> game
       ;; (reduce merge)
       (merge-with - limit)
       vals
       vec
       (every? #(or (zero? %) (pos? %)))))

(defn find-max-colors [steps]
  (println "steps:" steps)
  (loop [s steps max-color {}]
    (if (empty? s)
      max-color
      (recur (rest s) (merge-with max max-color (first s))))))

(defn check-colors [str]
  (let [steps-str (string/split (string/trim str) #";")
        steps (map find-step steps-str)]
    (loop [s steps result []]
      (if (empty? s)
          result
          (recur (rest s) (conj result (check (first s))))))))


(defn find-game-num [str]
  (let [str-split (string/split (string/trim str) #" ")
        [game num] str-split]
    (parse-long num)))

(defn check-game-data [str]
  (let [[num-str color-str] (string/split (string/trim str) #":")
        game-num (find-game-num num-str)
        checks (check-colors color-str)]
    (println "checks:" checks)
    (if (every? true? checks)
      game-num
      0)))

(defn get-game-data-part2 [str]
  (let [[num-str color-str] (string/split (string/trim str) #":")
        steps-str (string/split (string/trim color-str) #";")]
    (->> steps-str
		 (map find-step)
         find-max-colors
         vals
         (reduce *)
         )))

(def part-1-input "day2-1")
(def part-2-input "day2-2")
(def part-1-example "day2-1-example")

(defn part1 []
  (->> part-1-input
      util/read-input
      util/->line
      (map check-game-data)
      (reduce +)))

(defn part2 []
  (->> part-2-input
       util/read-input
       util/->line
       (map get-game-data-part2)
       (reduce +)))
