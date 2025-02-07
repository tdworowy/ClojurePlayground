(ns fwpd.core
  (:gen-class))

(def filename "suspects.csv")
(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer/parseInt (clojure.string/trim str)))

(def conversions {:name identity :glitter-index str->int})

(defn convert [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn glitter-filter-names [minimum-glitter records]
  (map :name (filter #(>= (:glitter-index %) minimum-glitter) records)))

(defn append-to-map [suspect list-of-maps]
  (cons suspect list-of-maps))

(defn validate? [record & keywords]
  (every? #(contains? record %) keywords))

(println (glitter-filter 3 (mapify (parse (slurp filename)))))
(println (glitter-filter-names 3 (mapify (parse (slurp filename)))))

(println (append-to-map {:name "Test Name" :glitter-index 2} (mapify (parse (slurp filename)))))
(println (validate?  {:name "test" :glitter-index 2} :name :glitter-index))
(println (validate? {:name "test"}  :name :glitter-index))
