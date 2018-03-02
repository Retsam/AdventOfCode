(ns day21.core
  (:gen-class))

(use 'clojure.java.io)
(require '[clojure.string :as s])

(def INPUT "abcdefgh")
(def INPUT2 "fbgdceah")
(def INPUT_FILE "input.txt")

(defn parse-command
  [line]
  (let [
    words (s/split line #" ")
    nth-int (comp read-string nth)
    ]
    (cond
      (= (first words) "swap")
        (if (= (second words) "letter")
          (list :SWAP_LETTER (nth words 2) (nth words 5))
          (list :SWAP_POS (nth-int words 2) (nth-int words 5))
        )
      (= (first words) "reverse") (list :REVERSE (nth-int words 2) (nth-int words 4))
      (= (first words) "rotate")
        (if (= (second words) "based")
          (list :ROTATE_BASED (nth words 6))
          (list :ROTATE_DIR (second words) (nth-int words 2))
        )
      (= (first words) "move") (list :MOVE (nth-int words 2) (nth-int words 5))
    )
  )
)

(defn swap-position
  [x a b]
  (if (<= a b)
    (str (subs x 0 a) (get x b) (subs x (inc a) b) (get x a) (subs x (inc b)))
    (swap-position x b a)
  )
)

(defn swap-letter
  [x a b]
  (swap-position x (s/index-of x a) (s/index-of x b)) )

(defn do-reverse
  [x a b]
  (str (subs x 0 a) (s/reverse (subs x a (inc b))) (subs x (inc b))))

(defn rotate-dir
  [x dir steps]
  (let [
    len (count x)
    r (if  ;Rotate left x is equivalent to rotate right (len-x)
      (= dir "left")
      (mod steps len)
      (- len (mod steps len))
    )
  ]
    (str (subs x r) (subs x 0 r))
  )
)

(defn rotate-based
  [x a]
  (let [
    i (inc (s/index-of x a))
  ]
    (rotate-dir x "right" (if (> i 4) (inc i) i))
  )
)

(defn unrotate-based
  [x a]
  (let [
    current-index (s/index-of x a)
    original-index
      (if (= current-index 0) 7
        (dec (if (even? (inc current-index))
          (/ (inc current-index) 2)
          (+ 4 (int (/ (inc current-index) 2)))
        )))
    dir (if (> current-index original-index) "left" "right")
    dist (Math/abs (- current-index original-index))
  ]
    (rotate-dir x dir dist)
  )
)

(defn move
  [x a b]
  (if (> b a)
    (str (subs x 0 a) (subs x (inc a) (inc b)) (get x a) (subs x (inc b)))
    (str (subs x 0 b) (get x a) (subs x b a) (subs x (inc a)))
  )
)

(defn apply-command
  [x command]
  ; (println x command)
  (let [
    [id a b] command
  ]
    (case id
      :SWAP_LETTER (swap-letter x a b)
      :SWAP_POS (swap-position x a b)
      :REVERSE (do-reverse x a b)
      :ROTATE_BASED (rotate-based x a)
      :ROTATE_DIR (rotate-dir x a b)
      :MOVE (move x a b)
    )
  )
)

(defn undo-command
  [x command]
  ; (println x command)
  (let [
    [id a b] command
  ]
    (case id
      :SWAP_LETTER (swap-letter x a b)
      :SWAP_POS (swap-position x a b)
      :REVERSE (do-reverse x a b)
      :ROTATE_BASED (unrotate-based x a)
      :ROTATE_DIR (rotate-dir x (if (= a "right") "left" "right") b)
      :MOVE (move x b a)
    )
  )
)


(defn with-input [fnToCall]
  (with-open [
    rdr (reader INPUT_FILE)
  ] (fnToCall (line-seq rdr)) ))



(defn -main
  ""
  []
  (with-input (fn
    [lines]
    ; (reduce apply-command INPUT (map parse-command lines))
    (reduce undo-command INPUT2 (reverse (map parse-command lines)))
  ))
)
