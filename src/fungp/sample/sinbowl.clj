;;; ### Sinbowl regression problem

(ns fungp.sample.sinbowl
  (:use fungp.core)
  (:use fungp.util)
  (:require clojure.pprint))

(defn sinbowl-function
  "The function to match"
  [x] (- (* 0.1 (abs x)) (sin x)))


(def sinbowl-functions
  "Functions and their arities"
  '[[+ 2][- 2][* 2][fungp.util/abs 1]
    [Math/sin 1][fungp.util/sdiv 2][inc 1][dec 1]])

(def sinbowl-parameters
  "Only one parameter for sinbowl"
  '[x])

(def sinbowl-numbers
  "Lots of number literals to use as terminals"
  '[-1 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 10])

(def test-range
  "The input test cases"
  (map #(- % 20) (range 40)))

(def sinbowl-actual
  "The output test cases"
  (map sinbowl-function test-range))

(defn sinbowl-fitness
  "Fitness function. Takes a tree, evals it, and returns a fitness/error score."
  [tree]
  (try
    (let [f (eval (list 'fn 'testfunc '[x] tree))
          results (map f test-range)]
      (reduce + (map off-by-sq sinbowl-actual results)))
    (catch Exception e (println e) (println tree))))

(defn sinbowl-report
  "Reporting function. Prints out the tree and its score"
  [tree fitness]
  (clojure.pprint/pprint (list 'fn '[a] tree))(print "\n")
  (print "Error:\t")(print fitness)(print "\n\n")
  (flush))

(defn test-sinbowl [n1 n2]
  (println (str "Test inputs: " (vec test-range)))
  (println (str "Test outputs: " (vec sinbowl-actual)))
  (println (str "Max generations: " (* n1 n2)))
  (println)
  (let [options {:iterations n1 :migrations n2 :num-islands 6 :population-size 50 :tournament-size 4 :mutation-probability 0.1
                 :max-depth 10 :terminals sinbowl-parameters :numbers sinbowl-numbers :fitness sinbowl-fitness
                 :functions sinbowl-functions :report sinbowl-report :adf-count 1}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (sinbowl-report tree score))))
  
