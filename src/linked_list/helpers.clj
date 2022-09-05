(ns linked-list.helpers
  (:require [linked-list.item :as item]))

(defn insert-item [start val insert-before]
  (println (format "Creating item: %s" val))
  (let [insert (fn [item before after] (if (insert-before val item) before after))
        f-start (fn [current _ inner-val] (insert current (item/new val current) (item/new (.value current) inner-val)))
        f-only (fn [current] (insert current (item/new val current) (item/new (.value current) (item/new val nil))))
        f-middle (fn [previous current next inner-val] (insert previous current (f-start current next inner-val)))
        f-last (fn [previous current] (insert previous current (f-only current)))
        f-empty (fn [] (item/new val nil))]
    (item/foldback' f-start f-only f-middle f-last f-empty identity nil start)))

(defn remove-item [start val value-equals]
  (let [remove (fn [item remove retain] (if (value-equals item val) remove retain))
        f-start (fn [current next inner-val] (remove current [true next] [(first inner-val) (item/new (.value current) (second inner-val))]))
        f-only (fn [current] (remove current [true nil] [false current]))
        f-middle (fn [previous current next inner-val] (remove previous [true current] (f-start current next inner-val)))
        f-last (fn [previous current] (remove previous [true current] (f-only current)))
        f-empty (fn [] [false nil])
        [removed result] (item/foldback' f-start f-only f-middle f-last f-empty identity nil start)]
    (if removed
      (println (format "Removed item: %s" val))
      (println (format "Item %s does not exist!" val)))
    result))

(defn remove-all [_]
  nil)

(defn print-loop [start]
  (let [item (atom start)]
    (while (some? @item)
      (reset! item (item/print-get-next @item)))))

(defn print-iterator [start]
  (if (some? start)
    (doseq [item (iterator-seq (.iterator start))] (item/print-get-next item))))

(defn print-recursive [start]
  (if (some? start)
    (print-recursive (item/print-get-next start))))

(defn print-fold [start]
  (let [f-some (fn [current _ accumulator] (format "%s%s, " accumulator (.value current)))
        f-last (fn [current accumulator] (format "%s%s\n" accumulator (.value current)))
        f-empty (fn [accumulator] accumulator)]
    (print (item/fold f-some f-last f-empty "" start))))

(defn print-foldback [start]
  (let [f-some (fn [current _ inner-val] (format "%s, %s" (.value current) inner-val))
        f-last (fn [current] (format "%s\n", (.value current)))
        f-empty (fn [] "")]
    (print (item/foldback f-some f-last f-empty identity start))))
