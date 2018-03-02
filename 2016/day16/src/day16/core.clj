(ns day16.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn invert
  "1101 -> 0100"
  [s]
  (defn invert-char [c] (if (= c \0) \1 \0))
  (apply str (reverse (map invert-char s )))
)

(defn gen-data-step
  [prev]
  (str prev "0" (invert prev))
)

(defn gen-data
  [initial-state len]
  (subs
    (some
      #(and (> (count %) len) %)
      (iterate gen-data-step initial-state))
    0 len))

(defn checksum
  [s]
  (if (odd? (count s))
    s
    ; (partition 2 s) )
    (checksum (apply str (map
      (fn [[a, b]] (if (= a b) \1 \0))
      (partition 2 s) ))))

)

(defn -main
  []
    (println (checksum (gen-data "01000100010010111" 35651584 )))
)
