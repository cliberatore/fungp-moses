(ns fungp.problems.regression
  (:use fungp.moses)
  (:use fungp.util)
  (:use clojure.pprint))

(use '[clojure.core.match :only (match)])
     ;'[fungp.tutorial :only (sample-report sample-parameters sample-fitness sample-functions number-literals sample-report)])

(defn normalize-leaves-regression [[func & operands]]
  (match [func operands]
         [(:or '+ '-) (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] a
         ['* (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
         ['sdiv (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
         :else (concat (list func) operands)
         )
)

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

(def sample-functions
  "Here's a vector of vectors consisting of [symbol arity] pairs. The symbol must resolve
   to a function, and the arity number is an integer representing how many arguments
   that function takes."
  '[[+ 2][- 2][* 2][fungp.util/abs 1]
    [fungp.util/sdiv 2] [Math/sin 1] [Math/cos 1] [inc 1] [dec 1]])
(def sample-parameters
  "This defines the parameters (or, in this case, parameter) to be used to eval the
  generated functions."
  ['a])


(def number-literals
  (map float (range 10)))

(def training-range
  "This defines the range of input to use as training input. The first argument for map here uses a shortcut for
   single-variable anonymous functions in Clojure."
  (map #(* 2 (- % 5)) (range 10)))

(defn match-func
  "For sake of convenience, we can define a function to generate the outputs we're attempting to match."
  [a] (abs (* 3 (* a a))))

(def actual-output
  "This defines the actual outputs we are trying to match."
  (map float (map match-func training-range)))

(defn sample-fitness
  "The fitness function; it takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (compile-tree tree sample-parameters) ;; compile using compile-tree
          results (map f training-range)] ;; map the function to the test range
      ;; then we compare the test results to the actual expected output
      ;; off-by-sq is a utility function that calculates difference squared
      (reduce + (map off-by-sq actual-output results)))
    ;; not necessary here, but this is how you might catch a possible exception
    (catch Exception e (println e) (println tree))))

(defn sample-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (pprint tree)
  (println (str "Error:\t" fitness "\n"))
  (flush))

(def regression-options {:iterations 15 :migrations 15
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report sample-report
                  :num-islands 6 :population-size 40
                  :moses-normalize normalize-tree-regression})

;(defn run-regression-tests []
;  (binding [fungp.moses/normalize-tree normalize-tree-regression]
;    (run-fungp-moses regression-options)))