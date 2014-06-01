(ns markov-chains.core)

; {
;  "if" { "the" 5 "you" 4}
;  "promise" { "for" 4 "to" 3}
; }

(def metamorphosis-text
  (slurp "resources/metamorphosis-text-only.txt"))

(defn words [string]
  (re-seq #"\S+" string))

(defn successive-words [word-seq]
  (group-by first (partition 2 1 word-seq)))

(defn process [word-seq]
  (into {} (map (fn [[k v]] [k (frequencies (map second v))])
                (successive-words word-seq))))

(defn first-words [word-map]
  (filter #(re-matches #"^[A-Z].*$" %) (keys word-map)))

(defn seed [word-map]
  (rand-nth (first-words word-map)))

(defn next-word [word word-map]
  (rand-nth (flatten (map (fn [[k v]] (repeat v k)) (get word-map word)))))

(defn recur-words [word word-map iterations]
  (cons word (if (< iterations 12)
               (let [successive (next-word word word-map)
                     next-iter (inc iterations)]
                 (recur-words successive word-map next-iter)))))

(defn -main []
  (let [word-map (process (words metamorphosis-text))
        stem (seed word-map)]
    (apply println (recur-words stem word-map 0))))
