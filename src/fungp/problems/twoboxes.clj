(ns fungp.problems.twoboxes
  (:use fungp.util)
  (:use fungp.moses)
  (:use fungp.test.util)
  (:use clojure.pprint)
  (:use clojure.stacktrace)
  (:use clojure.java.io))

(use '[clojure.core.match :only (match)])

(defn normalize-leaves-regression [[func & operands]]
  (match [func operands]
         [(:or '+ '-) (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] a
         ['- (:or (['L0 'L0] :seq) (['L1 'L1] :seq) (['H0 'H0] :seq) (['H1 'H1] :seq) (['W0 'W0] :seq) (['W1 'W1] :seq))] 0.0
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
  '[[+ 2][- 2][* 2][fungp.util/sdiv 2]])
(def sample-parameters
  "This defines the parameters (or, in this case, parameter) to be used to eval the
  generated functions."
  '[L0 W0 H0 L1 W1 H1])


(def number-literals
  '())

(def training-range
  '((2. 4. 7. 2. 5. 3.)
    (7. 10. 9. 10. 3. 1.)
    (10. 9. 4. 8. 1. 6.)
    (3. 9. 5. 1. 6. 4.)
    (4. 3. 2. 7. 6. 1.)
    (3. 3. 1. 9. 5. 4.)
    (5. 9. 9. 1. 7. 6.)
    (1. 2. 9. 3. 9. 2.)
    (2. 6. 8. 2. 6. 10.)))

(defn match-func
  "For sake of convenience, we can define a function to generate the outputs we're attempting to match."
  [[L0 W0 H0 L1 W1 H1]] (- (* L0 (* W0 H0)) (* L1 (* W1 H1))))

(def actual-output
  "This defines the actual outputs we are trying to match."
  (map float (map match-func training-range)))

(defn sample-fitness
  "The fitness function; it takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (eval (list 'fn [sample-parameters] tree)) ;; compile using compile-tree
          results (map f training-range)] ;; map the function to the test range
      ;; Here, we have to make sure that we don't overload our results, setting the intial
      ;; value to double. Clojure will start with an int if we don't tell it.
      ;(println results)
      (reduce + (double 0) (map off-by-sq actual-output results)))
    ;; not necessary here, but this is how you might catch a possible exception
    (catch Exception e
      (println e)
      (println "Sample paremeters:" sample-parameters)
      (println "Output: " (eval (list 'fn [sample-parameters] tree)))
      (println tree)
      (print-stack-trace *e))))

(defn sample-report
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush)
    (with-open [wrtr (writer (str "reports/twobox.txt." now) :append true)]
      (.write wrtr (str iteration "," best-fit ",(" (apply str best-tree) ")"))))

(def twobox-options {:iterations 1 :migrations 51
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report sample-report
                  :num-islands 6 :population-size 2000
                  :adf-count 2
                  :moses-normalize normalize-tree-regression})

(defn run-twobox []
  (with-open [w (writer (str "reports/twobox.txt." now))])
  (run-fungp-moses twobox-options))