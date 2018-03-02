(ns day20.core
  (:gen-class))

(require '[clojure.string :as str])

(use 'clojure.java.io)
(def INPUT_FILE "input.txt")
(def MAX_IP 4294967295)

(defn with-input [fnToCall]
  (with-open [
    rdr (reader INPUT_FILE)
  ] (fnToCall (line-seq rdr)) ))

(defn get-ranges
  [input-lines]
  (map
    (fn [line]
      (vec (map read-string (str/split line #"-")))
    )
    input-lines
  )
)

(defn sort-ranges
  [ranges]
  (sort
    #(compare (get %1 0) (get %2 0))
    ranges))

(defn combine-ranges
  [ranges]
  (let
    [
      [ranges last-range] (reduce (fn
        [[new-ranges range-a] range-b]
        (let
          [
            [a1 a2] range-a
            [b1 b2] range-b
          ]
          (if (> b1 (+ a2 1))
            [(conj new-ranges range-a) range-b]
            [new-ranges [a1 (max a2 b2)]]
          )
        )
      )
      [[] (first ranges)]
      (rest ranges) )
    ]
      (conj ranges last-range)
  )
)

(defn -main
  []
  (with-input
    (fn [input-lines]
      (let [
        ranges (combine-ranges (sort-ranges (get-ranges input-lines)))
        ;P1; [r0 r1] (first ranges)
      ]
        ;P1; (if (= r0 0) (+ r1 1) 0)
        (println ranges)
        (first (reduce
          (fn [[count prev] next]
            (let [[a1 a2] prev [b1 b2] next]
              [(+ count (- b1 a2 1)) next]
            )
          )
          [0 [nil -1]]
          (conj ranges [(+ MAX_IP 1) nil])
          ; range-starts
        ))
      )
    )
  )
)
