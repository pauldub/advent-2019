(ns advent-2019.day-4)

(def password-range [172930 683082])

(defn parse-password [p]
  (->> p str seq (map #(Character/digit % 10))))

(defn has-double? [digits]
  (let [count-digits (fn [[acc last-digit] digit]
                       (if (= last-digit digit)
                         [(update acc digit #(inc (or % 1))) digit]
                         [acc digit]))]
    (->> digits
         (reduce count-digits [{} 0])
         first vals
         (some #(= 2 %)))))

(defn only-increasing-digits? [digits] (apply <= digits))

(defn valid-password? [p]
  (let [digits (parse-password p)]
    (and (only-increasing-digits? digits)
         (has-double? digits))))

(defn generate-passwords []
  (->> password-range
       (apply range)
       (filter valid-password?)))
