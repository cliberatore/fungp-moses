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
    ;(with-open [wrtr (writer (str "reports/twobox.moses." now) :append true)]
      ;(.write wrtr (str iteration "," best-fit ",(let " (nth best-tree 1) " " (str (apply str (nth best-tree 2)))")\n"))))

(defn report-twobox-fungp
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush))
    ;(with-open [wrtr (writer (str "reports/twobox.fungp." now) :append true)]
      ;(.write wrtr (str iteration "," best-fit ",(let " (nth best-tree 1) " " (str (apply str (nth best-tree 2)))")\n"))))

(def twobox-moses-options {:iterations 1 :migrations 1
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report report-twobox-moses
                  :num-islands 6 :population-size 20
                  :adf-count 2
                  :moses-normalize normalize-tree-regression})

(def twobox-fungp-options {:iterations 1 :migrations 1
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report report-twobox-fungp
                  :num-islands 6 :population-size 20
                  :adf-count 2
                  :moses-normalize normalize-tree-regression})

(defn test-norm-twobox [iters]
  (with-open [wrtr (writer (str "reports/twobox.norm." now ".time") :append true)]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-fungp-moses twobox-moses-options)))]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n"))
      (if (> iters 0)
        (recur (- iters 1))
        runtime)))))

(defn test-fungp-twobox [iters]
  (with-open [wrtr (writer (str "reports/twobox.fungp." now ".time") :append true)]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-genetic-programming twobox-fungp-options)))]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n"))
      (if (> iters 0)
        (recur (- iters 1))
        runtime)))))