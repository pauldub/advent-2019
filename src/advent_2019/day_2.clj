(ns advent-2019.day-2
  (:require [clojure.string :as string]))

(def program (map #(Integer/parseInt %) (string/split (slurp "src/advent_2019/day_2.txt") #"[,\n]")))

(defn next-opcode [memory pc] (take 4 (drop (* pc 4) memory)))

(defn read-memory [memory addr]
  (when addr (nth memory addr 0)))

(defn execute-opcode [memory pc]
  (let [[code in-a in-b out] (next-opcode memory pc)
        a (read-memory memory in-a)
        b (read-memory memory in-b)]
    (case code
      1  (assoc memory out (+ a b))
      2  (assoc memory out (* a b))
      99 memory)))

(defn initialize-memory [memory noun verb]
  (-> (into [] memory)
      (assoc 1 noun)
      (assoc 2 verb)))

(defn execute-program [program noun verb]
  (let [memory (initialize-memory program noun verb)
        padded-memory (partition 4 4 (iterate constantly 0) memory)]
    (reduce execute-opcode
            memory
            (range (- (count padded-memory) 1)))))

(defn find-solution []
  (let [iterations (into [] (mapcat (fn [x] (map (fn [y] [x y]) (range 100))) (range 100)))]
    (filter #(= (nth % 0) 19690720)
            (map (fn [[x y]]
                   (execute-program program x y))
                 iterations))))

