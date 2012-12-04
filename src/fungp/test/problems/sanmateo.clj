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

(def sanmateo-moses-options {:iterations 1 :migrations 51
                  :terminals ant-terminals :max-depth 10
                  :numbers '() :fitness ant-fitness
                  :functions ant-functions :report report-sanmateo-moses
                  :num-islands 6 :population-size 700
                  :adf-count 2
                  :moses-normalize normalize-tree-sanmateo})

(def sanmateo-fungp-options {:iterations 1 :migrations 51
                  :terminals ant-terminals :max-depth 10
                  :numbers '() :fitness ant-fitness
                  :functions ant-functions :report report-sanmateo-fungp
                  :num-islands 6 :population-size 700
                  :adf-count 2})

(defn test-norm-sanmateo [iters]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-fungp-moses sanmateo-moses-options)))]
        (with-open [wrtr (writer (str "reports/sanmateo.norm." now ".time") :append true)]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))

(defn test-fungp-sanmateo [iters]
    (loop [iters iters]
      (let [runtime (time-body (rest (run-genetic-programming sanmateo-fungp-options)))]
        (with-open [wrtr (writer (str "reports/sanmateo.fungp." now ".time") :append true)]
         (.write wrtr (str (first runtime) "," (last (last runtime)) "\n")))
      (if (> iters 0)
        (recur (- iters 1))
        runtime))))