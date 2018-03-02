(ns bfs.core)

(defn bfs-search
  ([is-goal? get-children init]
    ;Default the hash-fn to just return nil, indicating no hashing
    (bfs-search is-goal? get-children (constantly nil) init)
  )

  ([is-goal? get-children hash-fn init]
    (loop [
      search-queue [init]
      hash-set #{}
    ]
      ; (println search-queue)
      (let [
        [next & rest] search-queue
        next-hash (hash-fn next) ;might be nil
      ]
        (if (and next-hash (contains? hash-set next-hash))
          ;Already seen, skip this one
          (do
            (println "Skip")
            (recur rest hash-set)
          )
          ;Otherwise test
          (if (or (nil? next) (is-goal? next))
            next
            (recur
              (into [] (concat rest (get-children next)))
              (conj hash-set next-hash)
            )))))))
