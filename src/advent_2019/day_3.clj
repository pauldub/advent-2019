(ns advent-2019.day-3
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-move [input]
   (let [[dir _] (string/split input #"[0-9]")
         [_ len] (string/split input #"[A-Z]")]
     [dir (Integer/parseInt len)]))

(defn move-position-cost [[x y cost] [direction length]]
  (->> (range (+ 1 length))
       (map #(case direction
               "U" [x            (+ y %1) (+ cost %1)]
               "D" [x            (- y %1) (+ cost %1)]
               "R" [(+ x %1)            y (+ cost %1)]
               "L" [(- x %1)            y (+ cost %1)]))))

(defn move-coordinates [line]
  (let [aggregate-positions (fn [positions move]
            (let [points (move-position-cost (last positions) move)]
              (concat positions points)))]
    (reduce aggregate-positions [[0 0 0]] line)))

(def input
  (->> (string/split (slurp "src/advent_2019/day_3.txt") #"\n")
       (map #(string/split % #","))
       (map (partial map parse-move))
       (map move-coordinates)))

(defn index-coordinate-cost [line]
  (let [associate-cost (fn [h point]
                         (let [[x y cost] point]
                           (assoc h [x y] cost)))]
    (reduce associate-cost {} line)))

(defn fastest-intersection [line-a line-b]
  (let [line-a-cost (index-coordinate-cost line-a)
        line-b-cost (index-coordinate-cost line-b)]
    (->> (set/intersection (into #{} (keys line-a-cost)) (into #{} (keys line-b-cost)))
         (map #(+ (line-a-cost %1) (line-b-cost %1)))
         (sort)
         (filter #(not (= 0 %)))
         first)))
