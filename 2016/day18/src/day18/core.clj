(ns day16.core
  (:gen-class))

(def input-str ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^")
; (def input-str "..^^.")
(def input (map #(= % \^) input-str))

(defn row-to-string [row]
  (clojure.string/join (map #(if % "^" ".") row))
)

(defn next-row
  [row]
  (map-indexed (fn
      [idx is-trap]
      (let [
        left (nth row (- idx 1) false)
        center (nth row idx)
        right (nth row (+ idx 1) false)
      ] (or
        (and left center (not right))
        (and center right (not left))
        (and left (not right) (not center))
        (and right (not center) (not left))
      ))
  ) row)
)

(defn -main
  "I don't do a whole lot ... yet."
  []
  (def rows (take 400000 (iterate next-row input)))

  (println (count (filter (complement identity) (flatten rows))))
)
