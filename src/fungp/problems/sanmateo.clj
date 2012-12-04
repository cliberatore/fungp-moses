(ns fungp.problems.sanmateo
  (:use fungp.sample.compile-ants))

(use '[clojure.core.match :only (match)])
     ;'[fungp.tutorial :only (sample-report sample-parameters sample-fitness sample-functions number-literals sample-report)])

(defn normalize-leaves-sanmateo [[func & operands]]
  { :post [#(not (nil? %))] }
  (match [func operands]
         ['do (:or ([(['left] :seq) (['right] :seq) a] :seq) ([a (['left] :seq) (['right] :seq)] :seq))] a
         ['do (:or ([(['right] :seq) (['left] :seq) a] :seq) ([a (['right] :seq) (['left] :seq)] :seq))] a
         :else (concat (list func) operands)
         ))

(defn normalize-tree-sanmateo [tree]
  { :post [#((println tree " " %) (nil? %))] }
  (let [[func & operands] tree]
    (if (> (count operands) 2) 
      (match [func operands]
         ['do (:or ([(['left] :seq) (['right] :seq) a] :seq) ([a (['left] :seq) (['right] :seq)] :seq))] a
         ['do (:or ([(['right] :seq) (['left] :seq) a] :seq) ([a (['right] :seq) (['left] :seq)] :seq))] a
         :else (concat (list func) 
				          (map #(cond (seq? %1) (normalize-tree-sanmateo %1)
				                      (symbol? %1) (symbol %1)) operands)))
      (concat (list func) operands))))
;  (cond (symbol? tree) tree
;		  (let [[func & operands] tree]
;			  (if (nil? operands) func
;				  ;if each operand is either a number or symbol, normalize
;				  (let [leaves? (reduce #(and %1 (or (number? %2) (symbol? %2))) true operands) ]
;				    (if leaves?
;				        (normalize-leaves-regression (concat (list func) operands))
;				      ;else, recurse
;				        (concat (list func) 
;				          (map #(cond (seq? %1) (normalize-tree-sanmateo %1)
;				                      (symbol? %1) (symbol %1)
;				                      (number? %1) (num %1)) operands))))))))

(defn select-knob [tree]
  ;(println "Selecting knob" tree (symbol? tree))
  (if (or (symbol? tree) (number? tree)) 'knob
      ;(println "Selecting knob from " tree)
		  (let [[func & operands] tree]
		  (if (nil? operands) 'knob
		    (concat (list func)
		          (loop [ops '()
		                 remaining operands]
		            ;(println ops remaining)
		            (cond (empty? remaining) ops
                      ( or (= '(left) (first remaining)) (= '(right) (first remaining)) (= '(move) (first remaining))) (concat ops (list 'knob) (rest remaining))
		                  (seq? (first remaining)) (concat ops (list (select-knob (first remaining))) (rest remaining))
		                  :else (recur (concat (list ops) (first remaining))
		                               (rest remaining)))))))))

(defn replace-knob [value tree]
  (if (and (symbol? tree) (= tree 'knob)) value
		  (let [[func & operands] tree]
		  (if (= func 'knob) value
		    (concat (list func)
		          (loop [ops '()
		                 remaining operands]
		             (cond (empty? remaining) ops
		                   (= 'knob (first remaining)) (concat ops (list value) (rest remaining))
		                   (seq? (first remaining)) (concat ops (list (replace-knob value (first remaining))) (rest remaining))
		                   :else (recur (concat ops (list (first remaining)))
		                               (rest remaining)))))))))

(defn optimize-knob [module-tree fitness parameters numbers]
  (last (loop [best [(Double/MAX_VALUE) '()]
         remaining (concat parameters numbers)]
        (if (empty? remaining) best
            (let [replaced-tree (list 'let (nth module-tree 1) (replace-knob (first remaining) (last module-tree)))
                  tree-fitness (fitness replaced-tree)]
              (recur (if (< tree-fitness (first best)) [tree-fitness replaced-tree] best)
                     (rest remaining)))))))

(defn optimize-tree-sanmateo [tree fitness parameters literals]
  ;Randomly select a single terminal from the tree and turn it into a knob
  (let [knob-module-tree (list 'let (nth tree 1) (select-knob (last tree)))]
    ;(println "in optimize-tree-regression, just called select-knob" knob-module-tree)
    (optimize-knob knob-module-tree fitness parameters literals)))