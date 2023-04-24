(ns linked-list.helpers
  (:require [linked-list.item :as item])
  (:import (linked_list.item Item)))

(defn insert-item [start val insert-before]
  (println (format "Creating item: %s" val))
  (let [insert #(if (insert-before val %1) %2 %3)
        f-start #(insert %1 (Item. val %1) (Item. (.value %1) %3))
        f-only #(insert % (Item. val %) (Item. (.value %) (Item. val nil)))
        f-middle #(insert %1 %2 (f-start %2 %3 %4))
        f-last #(insert %1 %2 (f-only %2))
        f-empty #(Item. val nil)]
    (item/foldback' f-start f-only f-middle f-last f-empty identity nil start)))

(defn remove-item [start val value-equals]
  (let [remove #(if (value-equals %1 val) %2 %3)
        f-start #(remove %1 [true %2] [(first %3) (Item. (.value %1) (second %3))])
        f-only #(remove % [true nil] [false %])
        f-middle #(remove %1 [true %2] (f-start %2 %3 %4))
        f-last #(remove %1 [true %2] (f-only %2))
        f-empty #([false nil])
        [removed result] (item/foldback' f-start f-only f-middle f-last f-empty identity nil start)]
    (println (format (if removed "Removed item: %s" "Item %s does not exist!") val))
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
  (let [f-some #(format "%s%s, " %3 (.value %1))
        f-last #(format "%s%s\n" %2 (.value %1))
        f-empty #(%)]
    (print (item/fold f-some f-last f-empty "" start))))

(defn print-foldback [start]
  (let [f-some #(format "%s, %s" (.value %1) %3)
        f-last #(format "%s\n", (.value %))
        f-empty #("")]
    (print (item/foldback f-some f-last f-empty identity start))))
