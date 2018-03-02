(ns day14.core
  (:gen-class))

(require 'digest)

(def SALT "yjdafjpo")

;(def nth-hash 
;  (memoize 
;    (fn [n] 
;      (digest/md5 (str SALT n)))   

(def nth-hash
  (memoize
    (fn [n]
      (nth 
        (drop 1 ;Skip the unhashed entry in the sequence
             (iterate digest/md5 (str SALT n)))
        2016))))
             
 

(defn hash-seq
  ([] (hash-seq 0))
  ([n] (cons {:hash (nth-hash n) :n n} (lazy-seq (hash-seq (+ n 1))))))
  

(defn has-triple
  "Checks whether the hash has some char three times in a row; returns the character"
  [hash]
  (let [[match] (re-find #"(.)\1\1" hash)]
    (if match (get match 0))))
    
(defn has-quint
  "Checks whether the hash has the character 'c' five times in a row"
  [c hash]
  (boolean (re-find (re-pattern (str c "{5}")) hash)))
      
(defn one-time-pad-key?
  "Checks if a given hash is a one-time key or not"
  [{:keys [n hash]}]
  (let [triple-char (has-triple hash)]
    (and 
      triple-char 
      (some (partial has-quint triple-char)
        (take 1000 (map :hash (hash-seq (+ n 1)))))
      n)))
           

(defn one-time-pad-key-seq
  "Returns indexes that match the otp criteria"
  ([] (one-time-pad-key-seq 0))
  ([start] (let [n (some one-time-pad-key? (hash-seq start))]
             (cons n (lazy-seq (one-time-pad-key-seq (+ n 1)))))))
             
  

(defn -main
  "Advent of Code2016, Day14"
  []
  (time (println (nth (one-time-pad-key-seq) 63))))
