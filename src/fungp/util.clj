(ns fungp.util)

(defn flip
  "Convenience function. Generates true with a probablilty of the
   given parameter (a number between 0 and 1)"
  [chance] (< (rand) chance))

(defn find-op
  "Find the entry in the function sequence for a given operator."
  [op funcs] (first (filter (fn [x] (= (:op x) op)) funcs)))

(defn conv-code
  "Take a tree and return a list of symbols based on the :name symbols."
  [tree funcs]
  (if (not (seq? tree))
    (if (fn? tree) (:name (find-op tree funcs)) tree)
    (map (fn [t] (conv-code t funcs)) tree)))

(defn off-by
  "Calculate error."
  [x y] (Math/abs (- x y)))

(defn on-key
  "Apply a function to a part of a record."
  [k r f] (assoc r k (f (k r))))
