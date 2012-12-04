(ns fungp.test.problems.sanmateo
  (:use fungp.problems.sanmateo)
  (:use fungp.sample.compile-ants)
  (:use fungp.test.util)
  (:use fungp.core)
  (:use fungp.moses)
  (:use criterium.core)
  (:use clojure.pprint)
  (:use clojure.java.io))

(defn report-sanmateo-moses
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))
    ;(with-open [wrtr (writer (str "reports/twobox.moses." now) :append true)]
      ;(.write wrtr (str iteration "," best-fit ",(let " (nth best-tree 1) " " (str (apply str (nth best-tree 2)))")\n"))))

(defn report-sanmateo-fungp
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))
    ;(with-open [wrtr (writer (str "reports/twobox.fungp." now) :append true)]
      ;(.write wrtr (str iteration "," best-fit ",(let " (nth best-tree 1) " " (str (apply str (nth best-tree 2)))")\n"))))

(def sanmateo-options {:iterations 1 :migrations 51
                  :terminals ant-terminals :max-depth 10
                  :numbers '() :fitness ant-fitness
                  :functions ant-functions
                  :num-islands 6 :population-size 250
                  :adf-count 2
                  :moses-normalize normalize-tree-sanmateo})

(def sanmateo-both-options (conj {:moses-normalization normalize-tree-sanmateo
                                    :moses-optimization optimize-tree-sanmateo} sanmateo-options))
(def sanmateo-norm-options (conj {:moses-normalization normalize-tree-sanmateo} sanmateo-options))
(def sanmateo-opt-options (conj {:moses-optimization optimize-tree-sanmateo} sanmateo-options))

(def sanmateo-options-test {:iterations 1 :migrations 2
                  :terminals ant-terminals :max-depth 10
                  :numbers '() :fitness ant-fitness
                  :functions ant-functions
                  :num-islands 1 :population-size 10
                  :adf-count 2})

(defn test-sanmateo [report-name samples options]
  (let [options (conj {:report (test-report report-name)} options)]
	  (loop [iters samples]
	    (let [runtime (time-body (rest (run-fungp-moses options)))]
	      (with-open [wrtr (writer (str "reports/" report-name "." now ".time") :append true)]
	        (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
	      (if (> iters 0)
	        (recur (- iters 1))
	        (with-open [wrtr (writer (str "reports/" report-name "." now ".generalization") :append true)]
	          (.write wrtr (str (run-ant (nth (last runtime) 1) TESTFOOD) "\n"))))))))