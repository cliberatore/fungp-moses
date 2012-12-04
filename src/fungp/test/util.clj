(ns fungp.test.util
  (:use clojure.pprint)
  (:use clojure.java.io))

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