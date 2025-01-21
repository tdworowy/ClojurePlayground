(ns basics.reduce_examples
  (:gen-class))

(defn update-map [some-map]
  "produce new map,with same keys but updated values"
  (reduce (fn [new-map [key val]] (assoc new-map key (inc val))) {} some-map))

(defn filter-map [some-map]
  (reduce (fn [new-map [key val]]
            (if (> val 4)
              (assoc new-map key val)
              new-map))
          {}
          some-map))

(println (update-map {:max 10 :min 1}))
(println (filter-map {:val1 10 :val2 2}))