(ns fungp.problems.regression
  :use 'fungp.tutorial)

(defn normalize-regression [[func & operands]]
  (match [func operands]
         [(:or '+ '-) (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] a
         ['* (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
         ['sdiv (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
         :else (concat (list func) operands)
         )
)

(defn normalize-destructure-regression [[func & operands :as tree]]
  ;(println func " " operands)
  (if (nil? operands) func
	  ;if each operand is either a number or symbol, normalize
	  (let [leaves? (reduce #(and %1 (or (number? %2) (symbol? %2))) true operands) ]
	    (if leaves?
	        (normalize-tree (concat (list func) operands))
	      ;else, recurse
	        (concat (list func) 
	          (map #(cond (list? %1) (normalize-destructure %1)
	                      (symbol? %1) (symbol %1)
	                      (number? %1) (num %1)) operands))))))

(def regression-options {:iterations 15 :migrations 15
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report sample-report
                  :num-islands 6 :population-size 40
                  :moses-normalize normalize-destructure-regression})