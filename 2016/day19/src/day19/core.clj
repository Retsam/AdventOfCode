(ns day19.core
  (:gen-class))

(def ELF_COUNT 3012210)
(def elves (vec (map (fn [_] true) (range 1 (+ ELF_COUNT 1)))))

(defn next-index [i] (mod (+ i 1) ELF_COUNT))

(defn next-elf
  [elves current-elf]
  (loop [
    index (+ current-elf 1)
  ]
    (if (get elves index)
      index
      (recur (next-index index))
    )))

(defn elf-to-remove
  [elves elf-count current-elf-index]

  ;PART 1
  ;(next-elf elves current-elf)

  (let [
    n (int (/ elf-count 2))
  ]
    (nth (iterate (partial next-elf elves) current-elf-index) n)
  )
)

(defn remove-elf
  [elves elf-count current-elf-index]
  (assoc elves (elf-to-remove elves elf-count current-elf-index) false) )

(defn -main
  "I don't do a whole lot ... yet."
  []

  ;Add one, since the answer is 1-indexed
  (println (str "The winner is " (+ 1 (loop [
    elves elves
    elf-count ELF_COUNT
    current-elf-index 0
  ]
    (println elf-count)
    (if (= elf-count 2)
      current-elf-index
      (let [
        rest-elves (remove-elf elves elf-count current-elf-index)
      ]
        (recur
          rest-elves
          (- elf-count 1)
          (next-elf rest-elves current-elf-index) )
      )
    )
  ))))
)
