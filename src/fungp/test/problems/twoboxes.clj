(ns fungp.test.problems.twoboxes
  (:use fungp.problems.twoboxes)
  (:use fungp.test.util)
  (:use fungp.core)
  (:use fungp.moses)
  (:use criterium.core)
  (:use clojure.pprint)
  (:use clojure.java.io))

(def ^:dynamic lastTime)

(defn test-normal-gp-twobox [iters]
  (with-open [wrtr (writer (str "reports/twobox." now ".time") :append true)]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-fungp-moses twobox-options)))]
         (.write wrtr (str runtime))
      (if (> iters 0)
        (recur (- iters 1))
        runtime)))))

(defn sample-report-test-gp
  "Reporting function. Prints out the tree and its score"
  [iteration best-tree best-fit]
	  (pprint best-tree)
	  (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	  (flush)
    (with-open [wrtr (writer (str "reports/twobox." now) :append true)]
      (.write wrtr (str iteration "," best-fit ",(let " (nth best-tree 1) " " (str (nth best-tree 2))")\n"))))