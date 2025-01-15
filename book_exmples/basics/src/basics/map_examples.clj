(ns basics.map_examples
  (:gen-class))

;map for multiple functions
(def sum #(reduce + %))
(def avg #(/ (sum %) (count %)))
(defn stats [numbers] (map #(% numbers) [sum count avg]))

(println (stats  [3 4 10]))