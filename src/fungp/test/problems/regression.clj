(ns fungp.test.problems.regression
  (:use fungp.problems.regression)
  (:use fungp.test.util)
  (:use fungp.core)
  (:use fungp.moses)
  (:use criterium.core)
  (:use clojure.pprint)
  (:use clojure.java.io))

(defn report-regression-moses
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))

(defn report-regression-fungp
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))

(def regression-options {:iterations 5 :migrations 10
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions
                  :num-islands 6 :population-size 500
                  :adf-count 0 })

(def regression-options-test {:iterations 1 :migrations 10
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions
                  :num-islands 1 :population-size 10
                  :adf-count 0
                  :moses-normalization normalize-tree-regression
                  :moses-optimization optimize-tree-regression})

(def regression-both-options (conj {:moses-normalization normalize-tree-regression
                                    :moses-optimization optimize-tree-regression} regression-options))
(def regression-norm-options (conj {:moses-normalization normalize-tree-regression} regression-options))
(def regression-opt-options (conj {:moses-optimization optimize-tree-regression} regression-options))

(defn test-fungp [report-name samples options]
  (let [options (conj {:report (test-report report-name)} options)]
	  (loop [iters samples]
	    (let [runtime (time-body (rest (run-fungp-moses options)))]
	      (with-open [wrtr (writer (str "reports/" report-name now ".time") :append true)]
	        (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
	      (if (> iters 0)
	        (recur (- iters 1))
	        runtime)))))