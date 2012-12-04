(ns fungp.test.problems.twoboxes
  (:use fungp.problems.twoboxes)
  (:use fungp.test.util)
  (:use fungp.core)
  (:use fungp.moses)
  (:use criterium.core)
  (:use clojure.pprint)
  (:use clojure.java.io))

(defn report-twobox-moses
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))

(defn report-twobox-fungp
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))

(def twobox-options {:iterations 5 :migrations 10
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report report-twobox-moses
                  :num-islands 6 :population-size 500
                  :adf-count 2 })

(def twobox-options-test {:iterations 5 :migrations 1
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report report-twobox-moses
                  :num-islands 1 :population-size 100
                  :adf-count 2 :moses-optimization optimize-tree-regression})

(def twobox-both-options (conj {:moses-normalization normalize-tree-regression
                                :moses-optimization optimize-tree-regression} twobox-options))
(def twobox-norm-options (conj {:moses-normalization normalize-tree-regression} twobox-options))
(def twobox-opt-options (conj {:moses-optimization optimize-tree-regression} twobox-options))

(defn test-both-twobox [iters]
  (loop [iters iters]
    (let [runtime (time-body (rest (run-fungp-moses twobox-both-options)))]
      (with-open [wrtr (writer (str "reports/twobox.both." now ".time") :append true)]
        (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))

(defn test-opt-twobox [iters]
  (loop [iters iters]
    (let [runtime (time-body (rest (run-fungp-moses twobox-opt-options)))]
      (with-open [wrtr (writer (str "reports/twobox.opt." now ".time") :append true)]
        (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))

(defn test-norm-twobox [iters]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-fungp-moses twobox-norm-options)))]
        (with-open [wrtr (writer (str "reports/twobox.norm." now ".time") :append true)]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))

(defn test-fungp-twobox [iters]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-genetic-programming twobox-options)))]
        (with-open [wrtr (writer (str "reports/twobox.fungp." now ".time") :append true)]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))