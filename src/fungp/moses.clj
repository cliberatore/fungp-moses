(ns fungp.moses)

;normalizes a given tree. Input guaranteed to be either a terminal, number, or 
(def ^:dynamic normalize-tree)

(def ^:dynamic optimize-tree)

(defn normalize-module-tree [module-tree]
  { :pre [(bound? normalize-tree)] }
	(concat
	  (take 2 module-tree)
    ;If the body function is actually a list, try to normalize it. It could just be a terminal or number
    ;otherwise, just return the body of the function
    (if (list? (nth module-tree 2))
      (list (normalize-tree (nth module-tree 2)))
      (list (nth module-tree 2)))))

(defn normalize-population [population]
  (map #(normalize-module-tree %) population))

;(defn optimize-population [population])
;  ;(map #(optimize-module-tree %) population))

(defn run-fungp-moses
  "This is the entry function for running the FunGP library using the Moses algorithm. Call this with a map of the parameters to run the genetic programming algorithm."
  [{:keys [iterations migrations num-islands population-size tournament-size mutation-probability
           mutation-depth max-depth terminals functions numbers fitness report adf-arity adf-count
           adl-count adl-limit moses-normalization moses-optimization]
       ;; the :or block here specifies default values for some of the arguments
   :or {tournament-size 5 mutation-probability 0.1 mutation-depth 6 adf-arity 1 adf-count 0
        adl-count 0 adl-limit 25 numbers [] moses-normalization (fn [x] x) moses-optimization (fn [x] x)}}]
  (let [options {:iterations iterations :migrations migrations
                 :num-islands num-islands :population-size population-size
                 :tournament-size tournament-size :mutation-probability mutation-probability
                 :max-depth max-depth :terminals terminals
                 :functions functions :numbers numbers
                 :fitness fitness :report report
                 :adf-arity adf-arity :adf-count adf-count
                 :adl-count adl-count :adl-limit adl-limit
                 }]
    (binding [moses true
              fungp.moses/normalize moses-normalization
              fungp.moses/optimize moses-optimization] (run-genetic-programming options))))