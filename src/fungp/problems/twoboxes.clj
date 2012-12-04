(ns fungp.problems.twoboxes
  (:use fungp.util)
  (:use fungp.moses)
  (:use fungp.test.util)
  (:use clojure.pprint)
  (:use clojure.stacktrace)
  (:use clojure.java.io))

(use '[clojure.core.match :only (match)])

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
      (.write wrtr (str iteration "," best-fit ",(let " (str (nth best-tree 1)) " " (str (nth best-tree 2)) ")\n"))))

(defn normalize-leaves-regression [[func & operands]]
  (println "Normalize leaves regression")
	(match [func operands]
	       [(:or '+ '-) (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] a
	       ['- (:or (['L0 'L0] :seq) (['L1 'L1] :seq) (['H0 'H0] :seq) (['H1 'H1] :seq) (['W0 'W0] :seq) (['W1 'W1] :seq))] 0.0
	       ['* (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
	       ['fungp.util/sdiv (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
	       :else 
           (cond (and (= (count operands) 2) (= func 'fungp.util/sdiv) (= (first operands) (last operands))) 1.0
                 (and (= (count operands) 2) (= func '-)    (= (first operands) (last operands))) 0.0
                 :else (concat (list func) operands))))

(defn normalize-tree-regression [tree]
  { :post [#((println tree " " %) (nil? %))] }
  ;(println "Normalize-tree-regression")
  (if (symbol? tree) tree
		  (let [[func & operands] tree]
			  (if (nil? operands) func
				  ;if each operand is either a number or symbol, normalize
				  (let [leaves? (reduce #(and %1 (or (number? %2) (symbol? %2))) true operands) ]
				    (if leaves?
				        (normalize-leaves-regression (concat (list func) operands))
				      ;else, recurse
				        (concat (list func) 
				          (map #(cond (seq? %1) (normalize-tree-regression %1)
				                      (symbol? %1) (symbol %1)
				                      (number? %1) (num %1)) operands))))))))

(defn select-knob [tree]
  (if (symbol? tree) 'knob
      ;(println "Selecting knob from " tree)
		  (let [[func & operands] tree]
		  (if (nil? operands) 'knob
		    (concat (list func)
		          (loop [ops '()
		                 remaining operands]
		            (println ops remaining)
		            (cond (empty? remaining) ops
		                  ( or (symbol? (first remaining)) (number? (first remaining))) (concat ops (list 'knob) (rest remaining))
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
  (println "In optimize-knob")
  (last (loop [best [(Double/MAX_VALUE) '()]
         remaining (concat parameters numbers)]
        (if (empty? remaining) best
            (let [replaced-tree (list 'let (nth module-tree 1) (replace-knob (first remaining) (last module-tree)))
                  tree-fitness (sample-fitness replaced-tree)]
              (recur (if (< tree-fitness (first best)) [tree-fitness replaced-tree] best)
                     (rest remaining)))))))

(defn optimize-tree-regression [tree fitness parameters literals]
  (println "in optimize-tree regression" tree)
  ;Randomly select a single terminal from the tree and turn it into a knob
  (let [knob-module-tree (list 'let (nth tree 1) (select-knob (list (last tree))))]
    (println "in optimize-tree-regression, just called select-knob" knob-module-tree)
    (optimize-knob knob-module-tree fitness parameters literals)))

;(defn run-twobox []
;  (with-open [w (writer (str "reports/twobox.txt." now))])
;  (run-fungp-moses twobox-options))