(ns day15.core
  (:use clojure.pprint)
  (:gen-class))

(defn normalize-start
  "Adds a disk index (plus one) to its start to create a normalized start"
  [idx disk]
  (assoc disk :start (mod (+ 1 idx (:start disk)) (:size disk)))
)

(def disks (map-indexed normalize-start [
  {:size 13 :start 11},
  {:size 5  :start 0},
  {:size 17 :start 11},
  {:size 3  :start 0},
  {:size 7  :start 2},
  {:size 19 :start 17},
  ;Part 2 only:
  {:size 11 :start 0}
]))

(defn passes-all?
  [t]
  (every?
    #(=
        0
        (mod (+ t (:start %)) (:size %) ))
    disks))

(defn -main
  "Advent of code day 15"
  []
  (pprint disks)
  (print (first (filter passes-all? (range))))
)
