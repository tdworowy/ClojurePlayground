(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n  2))))))

(println (take 10 (even-numbers)))