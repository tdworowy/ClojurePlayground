(ns shop.core
  (:gen-class))

(def order-details
  {:name "Mitchard Blimmos"
   :email "mitchard.blimmonsgmail.com"})

(def order-details-validations
  {:name ["Pleas enter a name" not-empty]
   :email ["Pleas enter a email address" not-empty
           "your email address doesn't look like en email address"
           #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate)) (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(let [errors (validate order-details order-details-validations)]
  (if (empty? errors)
    (println :success)
    (println :failure errors)))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(println (macroexpand
          '(if-valid order-details order-details-validations my-error-name
                     (println :success)
                     (println :failure my-error-name))))

(let*
 [my-error-name (validate order-details order-details-validations)]
 (if (clojure.core/empty? my-error-name)
   (println :success)
   (println :failure my-error-name)))