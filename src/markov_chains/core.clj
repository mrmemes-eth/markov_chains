(ns markov-chains.core)

(defn word-seq [string]
  (re-seq #"\S+" string))

(defn successive-words [word-seq]
  (group-by first (partition 2 1 word-seq)))

(defn build-word-map [word-seq]
  (into {} (map (fn [[k v]] [k (frequencies (map second v))])
                (successive-words word-seq))))

(defn first-words [word-map]
  (filter #(re-matches #"^[A-Z].*$" %) (keys word-map)))

(defn get-start-word [word-map]
  (rand-nth (first-words word-map)))

(defn get-next-word [word word-map]
  (rand-nth (flatten (map (fn [[k v]] (repeat v k)) (get word-map word)))))

(defn build-phrase [word word-map]
  (cons word (if (not (re-matches #".*[?!.]" word))
               (let [next-word (get-next-word word word-map)]
                 (build-phrase next-word word-map)))))

(defn -main [doc]
  (let [word-map (build-word-map (word-seq (slurp doc)))
        start-word (get-start-word word-map)]
    (apply println (build-phrase start-word word-map))))
