(ns hobbit.core
  (:gen-class))

(def asym-hobbit-body-parts [{:name "head" :size 3} {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1} {:name "mouth" :size 1}
                             {:name "nose" :size 1} {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3} {:name "left-uperarm" :size 3}
                             {:name "chest" :size 10} {:name "back" :size 10}
                             {:name "left-forearm" :size 3} {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1} {:name "left-heand" :size 2}
                             {:name "left-knee" :size 2} {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3} {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(def asym-multi-d-hobbit-body-parts [{:name "head" :size 3} {:name "1-eye" :size 1}
                                     {:name "1-ear" :size 1} {:name "mouth" :size 1}
                                     {:name "nose" :size 1} {:name "neck" :size 2}
                                     {:name "1-shoulder" :size 3} {:name "1-uperarm" :size 3}
                                     {:name "chest" :size 10} {:name "back" :size 10}
                                     {:name "1-forearm" :size 3} {:name "abdomen" :size 6}
                                     {:name "1-kidney" :size 1} {:name "1-heand" :size 2}
                                     {:name "1-knee" :size 2} {:name "1-thigh" :size 4}
                                     {:name "1-lower-leg" :size 3} {:name "1-achilles" :size 1}
                                     {:name "1-foot" :size 2}])

(defn matching-part [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-") :size (:size part)})

(defn matching-part-number [part number]
  {:name (clojure.string/replace (:name part) #"^1-" (str number "-")) :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts final-body-parts []]
    (if (empty? remaining-asym-parts) final-body-parts
        (let [[part & remaining] remaining-asym-parts]
          (recur remaining (into final-body-parts (set [part (matching-part part)])))))))

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part] (into final-body-parts (set [part (matching-part part)]))) [] asym-body-parts))

(defn better-symmetrize-body-parts-number
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts number]
  (mapcat (fn [i]
            (reduce (fn [final-body-parts part]
                      (into final-body-parts (set [part (matching-part-number part i)])))
                    []
                    asym-body-parts))
          (range 1 number)))

(defn hit [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))
