(ns advent-of-code.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input
  [file]
  (slurp (io/resource file)))

(defn ->line
  [str]
  (str/split-lines str))
