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
         ['* (:or ([1.0 a] :seq) ([a 1.0] :seq))] a
         ['fungp.util/sdiv (:or ([0.0 a] :seq) ([a 0.0] :seq) ([0 a] :seq) ([a 0] :seq))] 0.0
         ['fungp.util/sdiv ([a 1.0] :seq)] a
         :else
           ;Core.match can't do instances where we have something like (- X X) = 0.0
           ;We do those manually, here.
           (cond (and (= (count operands) 2) (= func 'fungp.util/sdiv) (= (first operands) (last operands))) 1.0
                 (and (= (count operands) 2) (= func '-)    (= (first operands) (last operands))) 0.0
                 :else (concat (list func) operands))))

(defn normalize-tree-regression [tree]
  { :post [#((println tree " " %) (nil? %))] }
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
  ;(println "Optimizing knob")
  (last (loop [best [(Double/MAX_VALUE) '()]
         remaining (concat parameters numbers)]
        (if (empty? remaining) best
            (let [replaced-tree (list 'let (nth module-tree 1) (replace-knob (first remaining) (last module-tree)))
                  tree-fitness (fitness replaced-tree)]
              (recur (if (< tree-fitness (first best)) [tree-fitness replaced-tree] best)
                     (rest remaining)))))))

(defn optimize-tree-regression [tree fitness parameters literals]
  ;Randomly select a single terminal from the tree and turn it into a knob
  (let [knob-module-tree (list 'let (nth tree 1) (select-knob (last tree)))]
    ;(println "in optimize-tree-regression, just called select-knob" knob-module-tree)
    (optimize-knob knob-module-tree fitness parameters literals)))

(def sample-functions
  "Here's a vector of vectors consisting of [symbol arity] pairs. The symbol must resolve
   to a function, and the arity number is an integer representing how many arguments
   that function takes."
  '[[+ 2][- 2][* 2][fungp.util/sdiv 2] [Math/sin 1]])
(def sample-parameters
  "This defines the parameters (or, in this case, parameter) to be used to eval the
  generated functions."
  ['X])


(def number-literals
  '(1.0))

(def training-range
  "This defines the range of input to use as training input. The first argument for map here uses a shortcut for
   single-variable anonymous functions in Clojure."
  (map #(/ (* % Math/PI) 6) (range 12)))

(defn match-func
  "For sake of convenience, we can define a function to generate the outputs we're attempting to match."
  [a] (Math/cos (* 2 a)))

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

;(defn run-regression-tests []
;  (binding [fungp.moses/normalize-tree normalize-tree-regression]
;    (run-fungp-moses regression-options)))