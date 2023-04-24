(ns linked-list.main
  (:require [linked-list.helpers :as helpers]))

(def valid-regex (re-pattern #"^(0|-?[1-9][0-9]*|[A-Za-z][0-9A-Z_a-z]*)$"))
(def number-regex (re-pattern #"^-?[0-9]+$"))

(defn is-valid-string [str]
  (some? (re-matches valid-regex str)))

(defn is-number-string [str]
  (some? (re-matches number-regex str)))

(defn insert-before [^String val item]
  (let [^String other-val (.value item)]
    (if (and (is-number-string val) (is-number-string other-val))
      (< (.compareTo (BigInteger. val) (BigInteger. other-val)) 1)
      (< (.compareTo val other-val) 1))))

(defn value-equals [item val]
  (.equals (.value item) val))

(defn main [begin start]
  (if (not begin)
    (println))
  (println "Awaiting input...")
  (let [input (read-line)
        main' #(main false %)
        parse-fail #(do
                     (println "\nCould not parse input!")
                     (main' %))]
    (if (.isEmpty input)
      (do
        (println "\nProgram terminated!")
        (helpers/remove-all start))
      (if (= (.charAt input 0) \~)
        (if (= (.length input) 1)
          (do
            (println "\nDeleting list...")
            (main' (helpers/remove-all start)))
          (let [substr (.substring input 1)]
           (if (is-valid-string substr)
             (do
               (println "\nRemoving item...")
               (main' (helpers/remove-item start substr value-equals)))
             (parse-fail start))))
        (let [print-list #(do
                           (println %1)
                           (%2 start)
                           (main' start))]
          (case input
            "l" (print-list "\nLoop print..." helpers/print-loop)
            "i" (print-list "\nIterator print..." helpers/print-iterator)
            "a" (do
                  (println "\nArray print not implemented!")
                  (main' start))
            "r" (print-list "\nRecursive print..." helpers/print-recursive)
            "f" (print-list "\nFold print..." helpers/print-fold)
            "b" (print-list "\nFoldback print..." helpers/print-foldback)
            (if (is-valid-string input)
              (do
                (println "\nInserting item...")
                (main' (helpers/insert-item start input insert-before)))
              (parse-fail start))))))))

(main true nil)
