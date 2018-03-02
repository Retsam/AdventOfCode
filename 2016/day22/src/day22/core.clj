(ns day22.core
  (:gen-class))

(use 'clojure.java.io)
(require '[clojure.string :as s])

(require 'bfs.core)
(refer 'bfs.core)

(def INPUT_FILE "input.txt")
(def GOAL_DATA [36 0])
(def DEST_NODE [0 0])

; UTILITIES
(defn all-pairs
  [c]
  (reduce
    (fn
      [results [i el]]
      (into results (map #(vector el %) (drop (inc i) c))) )
    []
    (map-indexed vector c)
))

(defn with-input [fnToCall]
  (with-open [
    rdr (reader INPUT_FILE)
  ] (fnToCall (line-seq rdr)) ))

; INPUT PARSING
(defn parse-bytes
  [bytes-string]
  (read-string (apply str (drop-last 1 bytes-string)))
)

(defn parse-coords
  [coords-string]
  (vec (map read-string (drop 1 (re-matches #"/dev/grid/node-x(\d+)-y(\d+)" coords-string)))) )

(defn parse-line
  [line]
  (let [
    parts (s/split line #"\s+")
    coords (parse-coords (nth parts 0))
  ]
    [coords {
      :goal-data?  (= 0 (compare GOAL_DATA coords))
      :size        (parse-bytes (nth parts 1))
      :used        (parse-bytes (nth parts 2))
      :avail       (parse-bytes (nth parts 3))
    }]))

; PREDICATES

(defn viable-pair?
  [node-a node-b]
  (and
    (not (zero? (:used node-a)))
    (< (:used node-a) (:avail node-b)) ))

(defn goal?
  [grid]
  (:goal-data? (grid DEST_NODE))
)

; GRID LOGIC

(defn get-neighbors
  [grid [x y]]
  (filter
    (partial contains? grid)
    [[(inc x) y], [(dec x) y] [x (inc y)] [x (dec y)]] ))

(defn get-viable-neighbors
  [grid coord-a]
  (filter
    (fn [coord-b]
      (viable-pair?
        (grid coord-a)
        (grid coord-b) ))
    (get-neighbors grid coord-a)))

; MOVE LOGIC

(defn move ;move data from a to b
  [node-a node-b]
  (assert (viable-pair? node-a node-b))

  (let [
    new-node-a (assoc node-a
      :used 0
      :avail (:size node-a)
      :goal-data? false )
    new-node-b (assoc node-b
      :used (+ (:used node-b) (:used node-a))
      :avail (- (:avail node-b) (:used node-a))
      :goal-data? (:goal-data? node-a) )
  ]
    [new-node-a new-node-b])
)

(defn do-move
  [grid [coords-a coords-b]]
  (let [
    [new-node-a new-node-b] (move (grid coords-a) (grid coords-b))
  ]
    (assoc grid
      coords-a new-node-a
      coords-b new-node-b)))

(defn get-possible-moves
  [grid]
  (mapcat
    (fn [[coord _]]
      (map #(vector coord %) (get-viable-neighbors grid coord))
    )
    grid
  ))

(defn do-moves
  [grid moves]
  (reduce (fn [grid move] (do-move grid move)) grid moves)
)

(with-input (fn
  [lines]
  (def grid (into {} (map parse-line (drop 1 lines))))
))

;type coord = [x y]
;type move [coord coord]   ;[[x y] [x y]]
;type moves [move...]      ;[[[x y] [x y]] [[x y] [x y]]]

(defn get-children ; moves -> [moves]  ; [[[[x y] [x y]] [[x y] [x y]]]]
  [grid moves]
  (map (partial conj moves)
      (get-possible-moves (do-moves grid moves)))
)


(defn -main
  ""
  []
  (def test-count 0)
  (bfs-search
    ;goal?
    (fn [moves]
      (println "Testing " moves)
      (def test-count (inc test-count)) (goal? (do-moves grid moves))
    )
    ;get-children;
    (partial get-children grid)
    ;hash-fn
    (fn [moves] (hash (do-moves grid moves)))
    ;init
    []
  )
)
