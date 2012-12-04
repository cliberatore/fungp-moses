(ns fungp.test.util
  (:use clojure.pprint)
  (:use clojure.java.io)
  (:use criterium.core)
  (:use fungp.moses))

(defn ts [] (.format (new java.text.SimpleDateFormat "YYYYMMdd.HHmmss") (new java.util.Date)))

(def now (ts))

(defn test-report
  "Reporting function. Prints out the tree and its score, to stdout and the report specified"
  [report-name]
  (with-open [wrtr (writer (str "reports/" report-name "." now ".report") :append true)]
    (.write wrtr (str "Iteration,Best-tree\n")))
  (fn 
    [iteration best-tree best-fit]
      (println iteration best-tree best-fit)
      (with-open [wrtr (writer (str "reports/" report-name "." now ".report") :append true)]
        (.write wrtr (str iteration "," best-fit "\n")))
	    (pprint best-tree)
	    (println (str "Iteration: " iteration "\tError:\t" best-fit "\n"))
	    (flush)))

(defn test-fungp [report-name samples options]
  (let [options (conj {:report (test-report report-name)} options)]
	  (loop [iters samples]
	    (let [runtime (time-body (rest (run-fungp-moses options)))]
	      (with-open [wrtr (writer (str "reports/" report-name "." now ".time") :append true)]
	        (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
	      (if (> iters 0)
	        (recur (- iters 1))
	        runtime)))))