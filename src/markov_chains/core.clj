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

(defn -main [word]
   (println (get (process (words metamorphosis-text)) word)))
