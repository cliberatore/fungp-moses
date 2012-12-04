(ns fungp.problems.sanmateo
  (:use fungp.sample.compile-ants))

(use '[clojure.core.match :only (match)])
     ;'[fungp.tutorial :only (sample-report sample-parameters sample-fitness sample-functions number-literals sample-report)])

(defn normalize-leaves-sanmateo [[func & operands]]
  (match [func operands]
         ['do (:or ([(['left] :seq) (['right] :seq) a] :seq) ([a (['left] :seq) (['right] :seq)] :seq))] a
         ['do (:or ([(['right] :seq) (['left] :seq) a] :seq) ([a (['right] :seq) (['left] :seq)] :seq))] a
         :else (concat (list func) operands)
         ))

(defn normalize-tree-sanmateo [[func & operands :as tree]]
  (if (nil? operands) func
	  ;if each operand is either a number or symbol, normalize
	  (let [leaves? (reduce #(and %1 (or (number? %2) (symbol? %2))) true operands) ]
	    (if leaves?
	        (normalize-leaves-sanmateo (concat (list func) operands))
	      ;else, recurse
	        (concat (list func) 
	          (map #(cond (list? %1) (normalize-tree-sanmateo %1)
	                      (symbol? %1) (symbol %1)
	                      (number? %1) (num %1)) operands))))))