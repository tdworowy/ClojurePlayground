(ns playsync.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))
;(def echo-chan (chan))
;(go (println (<! echo-chan)))
;(>!! echo-chan "ketchup1")
;
;;(def echo-buffer (chan 2))
;;(go (println (<! echo-buffer)))
;;(>!! echo-buffer "ketchup2")
;;(>!! echo-buffer "ketchup3")
;;(>!! echo-buffer "ketchup4")
;
;
;(def hi-chan (chan))
;(doseq [n (range 1000)]
;  (go (>! hi-chan (str "hi " n)))
;  )

(defn hot-dog-machine []
  (let [in (chan)
        out (chan)]
    (go (<! in) ; Read from 'in', not 'int'
        (>! out "hot dog"))
    [in out]))

(defn hot-dog-machine-v2 [hot-dog-count]
  (let [in (chan) out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                (do (>! out "hot dog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in) (close! out))))) [in out]))

(let [[in out] (hot-dog-machine-v2 2)]
  (>!! in "pocket lint")
  (println (<!! out))
  (>!! in 3)
  (println (<!! out))
  (>!! in 3)
  (println (<!! out))
  (>!! in 3)
  (<!! out))

(defn upload
  [headshot c]
  (go (Thread/sleep (long (rand 1000)))
      (>! c headshot)))

(let [c1 (chan) c2 (chan) c3 (chan)]
  (upload "test1" c1)
  (upload "test2" c2)
  (upload "test3" c3)
  (let [[headshot channel] (alts!! [c1 c2 c3])]
    (println "sending notification for" headshot)))

(defn append-to-file
  "Write a string to the end of a file"
  [filename s]
  (spit filename s :append true))

(defn format-quote
  "Delineate the beginning and end of a quote because it's convenient"
  [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote
  "Retrieve a random quote and format it"
  []
  (format-quote (slurp "http://www.braveclojure.com/random-quote")))

(defn snag-quotes
  [filename num-quotes]
  (let [c (chan)]
    (go (while true (append-to-file filename (<! c))))
    (dotimes [n num-quotes] (go (>! c (random-quote))))))

(defn upper-caser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer
  [in]
  (go (while true (println (<! in)))))

(def in-chan (chan))
(def upper-caser-out (upper-caser in-chan))
(def reverser-out (reverser upper-caser-out))