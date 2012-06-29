(ns fungp.util)

(defn flip
  "Convenience function. Generates true with a probablilty of the
   given parameter (a number between 0 and 1)"
  [chance] (< (rand) chance))

(defn find-op
  "Find the entry in the function sequence for a given operator."
  [op funcs] (first (filter (fn [x] (= (:op x) op)) funcs)))

(defn abs [x] (if (< x 0) (* -1 x) x))

(defn off-by
  "Calculate error."
  [x y] (abs (- x y)))

(defn on-key
  "Apply a function to a part of a record."
  [k r f] (assoc r k (f (k r))))
