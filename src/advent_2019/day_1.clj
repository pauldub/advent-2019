(ns advent-2019.day-1
  (:require [clojure.string :as string]))

(def modules (map #(Integer/parseInt %) (string/split (slurp "src/advent_2019/day_1.txt") #"\n")))

(defn fuel-required [mass]
  (let [fuel-mass (-> mass (/ 3) (Math/floor) (- 2))]
    (if (< fuel-mass 0) 0 fuel-mass)))

(defn- sum [xs] (reduce + 0.0 xs))

(defn fuel-required-including-fuel [mass]
  (sum (drop 1 (take-while #(> %1 0) (iterate fuel-required mass)))))

(defn total-fuel-required []
  (->> modules
       (map fuel-required-including-fuel)
       (sum)))
