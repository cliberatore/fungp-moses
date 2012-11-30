(ns fungp.moses.util
  (:use fungp.moses))

(defn normalize-tree-regression [[func & operands :as tree]]
  (if (nil? operands) func
	  ;if each operand is either a number or symbol, normalize
	  (let [leaves? (reduce #(and %1 (or (number? %2) (symbol? %2))) true operands) ]
	    (if leaves?
	        (normalize-leaves-regression (concat (list func) operands))
	      ;else, recurse
	        (concat (list func) 
	          (map #(cond (list? %1) (normalize-tree-regression %1)
	                      (symbol? %1) (symbol %1)
	                      (number? %1) (num %1)) operands))))))