(ns day17.core
  (:gen-class))

(require 'digest)

(def PASS-CODE "pgflpeqp")

(defn bfs-search
  [is-goal?, get-children init]
  (loop [search-queue [init] ]
    ; (println search-queue)
    (let [[next & rest] search-queue]
      (if (or (nil? next) (is-goal? next))
        next
        (recur (into [] (concat rest (get-children next)))) ))))

(defn bfs-search-all
  [is-goal?, get-children init]
  (loop [search-queue [init] solutions []]
    (let [[next & rest] search-queue]
      (if (nil? next)
        solutions ;Done looping, checked everything
        (let [new-search-queue (into [] (concat rest (get-children next)))]
          (if (is-goal? next)
            (recur rest (into [] (conj solutions next)))
            (recur new-search-queue solutions) ))))))

(defn is-goal?
  [{:keys [x, y]}]
  (and (= x 3) (= y 3)))

(defn door-is-open
  [char]
  (> (int char) (int \a) ))

(defn parse-pathcode
  [pathcode]
  (let [hash (digest/md5 pathcode)] {
    :up (door-is-open (get hash 0))
    :down (door-is-open (get hash 1))
    :left (door-is-open (get hash 2))
    :right (door-is-open (get hash 3))
  }))


(defn get-children
  [node]
  (def children ())
  (let [
    {pathcode :pathcode x :x y :y } node
    open-doors (parse-pathcode pathcode)
    up-pathcode (str pathcode "U")
    down-pathcode (str pathcode "D")
    left-pathcode (str pathcode "L")
    right-pathcode (str pathcode "R")
  ]
    (if (and (:up open-doors)    (> y 0)) (def children (conj children (conj node {:y (- y 1) :pathcode up-pathcode} ))))
    (if (and (:down open-doors)  (< y 3)) (def children (conj children (conj node {:y (+ y 1) :pathcode down-pathcode} ))))
    (if (and (:left open-doors)  (> x 0)) (def children (conj children (conj node {:x (- x 1) :pathcode left-pathcode} ))))
    (if (and (:right open-doors) (< x 3)) (def children (conj children (conj node {:x (+ x 1) :pathcode right-pathcode} ))))

    children ))

(defn -main
  "Advent of Code Day 17"
  []
  (println
    (-
      (count (:pathcode (last (bfs-search-all is-goal? get-children {:pathcode PASS-CODE :x 0 :y 0}))))
      (count PASS-CODE) )))
