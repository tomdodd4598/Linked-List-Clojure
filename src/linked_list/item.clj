(ns linked-list.item
  (:import (java.util Iterator)))

(deftype ItemIterator [item]
  Iterator
  (hasNext [_]
    (some? @item))
  (next [_]
    (let [next @item]
      (reset! item (.next @item))
      next)))

(deftype Item [value next]
  Iterable
  (iterator [this]
    (ItemIterator. (atom this))))

(defn print-get-next [item]
  (let [next (.next item)]
    (print (format "%s%s" (.value item) (if (nil? next) "\n" ", ")))
    next))

(defn fold [f-some f-last f-empty accumulator item]
  (if (some? item)
    (let [next (.next item)]
      (if (some? next)
        (fold f-some f-last f-empty (f-some item next accumulator) next)
        (f-last item accumulator)))
    (f-empty accumulator)))

(defn fold' [f-start f-only f-middle f-last f-empty accumulator previous current]
  (if (some? current)
    (let [next (.next current)]
      (if (some? next)
        (let [newAccumulator (if (some? previous)
                               (f-middle previous current next accumulator)
                               (f-start current next accumulator))]
          (fold' f-start f-only f-middle f-last f-empty newAccumulator current next))
        (if (some? previous)
          (f-last previous current accumulator)
          (f-only current accumulator))))
    (f-empty accumulator)))

(defn foldback [f-some f-last f-empty generator item]
  (if (some? item)
    (let [next (.next item)]
      (if (some? next)
        (foldback f-some f-last f-empty #(generator (f-some item next %)) next)
        (generator (f-last item))))
    (generator (f-empty))))

(defn foldback' [f-start f-only f-middle f-last f-empty generator previous current]
  (if (some? current)
    (let [next (.next current)]
      (if (some? next)
        (let [newInner #(if (some? previous)
                         (f-middle previous current next %)
                         (f-start current next %))]
          (foldback' f-start f-only f-middle f-last f-empty (comp generator newInner) current next))
        (generator (if (some? previous) (f-last previous current) (f-only current)))))
    (generator (f-empty))))
