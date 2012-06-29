;;; Sample use
;;; ----------
;;;
;;; This demonstrates a sample use of **fungp**.
;;;
;;; Here's an example of output from the **test-gp** function:
;;;     
;;;     fungp :: Functional Genetic Programming in Clojure
;;;     Mike Vollmer, 2012
;;;     ==================================================
;;;     
;;;     Attempting to match this function:
;;;     (fn [a] (+ 1 (+ (sin a) a)))
;;;     
;;;     Lower numbers are better. Results shown are sum of
;;;     error. Best so far:
;;;     
;;;     Code:   (fn [a] (+ (cos -1) a))
;;;     Error:  84.6391402476599
;;;     
;;;     Code:   (fn [a] (- a -1))
;;;     Error:  76.4219514139867
;;;     
;;;     Code:   (fn [a] (+ (cos (cos (* a (sin a)))) (+ 
;;;     (sin a) a)))
;;;     Error:  26.815311352785372
;;;     
;;;     Code:   (fn [a] (+ (sin (sin (+ (+ 0 a) 0))) (+
;;;     (+ 0 a) (cos (sin (* (cos (cos (- 0 a))) 0))))))
;;;     Error:  8.111235956013786
;;;     
;;;     Done!
;;;     (fn [a] (+ (cos (sin (- a a))) (+ (sin a) a)))
;;;     Lowest error: 0.0
;;;     

(ns fungp.sample
  (:use fungp.core)
  (:use fungp.util))

;;; Safe versions of inverse and divide to avoid divide-by-zero errors.

(defn inv [x]
  (if (zero? x) 0
      (/ 1 x)))

(defn sdiv [x y]
  (if (zero? y) 0
      (/ x y)))

;;; java.lang.Math methods cannot be higher-order functions, so they must
;;; be wrapped in clojure functions.

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tan [x] (Math/tan x))

(defn ifeq [a b c d] (if (= a b) c d))
(defn ifnoteq [a b c d] (if (not (= a b)) c d))
(defn gte [a b c d] (if (>= a b) c d))


;;; Functions are defined in a vector of maps like this.
(def funcs [{:op * :arity 2 :name '*}
            {:op + :arity 2 :name '+} 
            {:op - :arity 2 :name '-}
            {:op sdiv :arity 2 :name '/}
            {:op inc :arity 1 :name 'inc} 
            {:op dec :arity 1 :name 'dec}
            {:op inv :arity 1 :name 'inv}
            {:op abs :arity 1 :name 'abs}
            {:op sin :arity 1 :name 'Math/sin}
            {:op cos :arity 1 :name 'Math/cos}
            {:op tan :arity 1 :name 'Math/tan}
            {:op ifeq :arity 4 :name 'ifeq}
            {:op ifnoteq :arity 4 :name 'ifnoteq}
            {:op gte :arity 4 :name 'gte}
            ])

(def symbols '[a])

(def lits ['Math/PI 'Math/E])

;;(def symbtest '(fn [a] (/ Math/E (* Math/PI (+ a (inv (sin a)))))))
(def symbtest '(fn [a]
                 (if (> a 0) (inv a)
                     (* a a))))

(def testfit (eval symbtest))

(def rtests (range -50 50))

(def testdata (map vector rtests))

(def actual (map testfit rtests))

(defn repfunc
  "Reporting function. This one is designed to only report when invoked in
   parallel-population."
  [{code :code error :error}]
  (flush)
  (print "Code:\t")(print code)(print "\n")
  (print "Error:\t")(print error)(print "\n\n")
  (flush))

(defn test-gp
  "A simple test function for fungp. It attempts to match a function by supplying
   training cases in the form of inputs and outputs generated from the function
   to be matched."
  [cycle iter]
  (println "fungp :: Functional Genetic Programming in Clojure")
  (println "Mike Vollmer, 2012")
  (println "==================================================\n")
  (println "Attempting to match this function:")
  (print symbtest)
  (println "\nLower numbers are better. Results shown are sum of error. Best so far:\n")
  (def results (run-gp {:gens iter :cycles cycle :term [-1 1]
                        :pop-size 12 :forest-size 250 :depth [2 3]
                        :symbols symbols :funcs funcs
                        :report {:repfunc repfunc  :reprate 1}
                        :tournament-size 5
                        :fit {:actual actual :tests testdata}}))
  (def best-result (:best results))
  (println "Done!")
  (print "Lowest error: ")(print (:error best-result))(print "\n")
  ;; return quoted list of best function
  (:best results))

(defn -main []
  (test-gp 160 4))
