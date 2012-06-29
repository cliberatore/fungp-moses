;;; Mike Vollmer, 2012, GPL
;;;
;;; [Project hosted on GitHub](https://github.com/probabilityZero/fungp)
;;;
;;; What is this?
;;; -------------
;;;
;;; **fungp** is a parallel genetic programming library implemented in the
;;; Clojure programming language, pronounced fun-gee-pee. The "fun" comes
;;; from functional, and because genetic programming can be fun! Also I'm
;;; bad at naming things.
;;;
;;; > There are only two hard things in Computer Science: cache invalidation,
;;; > naming things, and off-by-one errors.
;;; >
;;; > --- *Paraphrased from Phil Karlton*
;;; 
;;; The library is in its early stages right now, but it's usable. Currently it
;;; has the following features:
;;;
;;;  * Custom evaluation and reporting logic
;;;  * Parallelism: subpopulations run in native threads
;;;  * Evolve and test functions of multiple arities
;;;
;;; How do I use it?
;;; ----------------
;;;
;;; Call the **run-gp** function. See the source code for the full list of 
;;; keyword parameters.
;;;
;;; Here's an example:
;;;
;;;     (run-gp {:gens iter :cycles cycle
;;;              :pop-size 6 :forest-size 50
;;;              :symbols symbols :funcs funcs
;;;              :term-max 1 :term-min -1
;;;              :max-depth 4 :min-depth 2
;;;              :repfunc repfunc  :reprate 1
;;;              :mutation-rate 0.1 :tournament-size 5
;;;              :actual actual :tests testdata})
;;;
;;; Functions are defined as a sequence of maps, each having keys :op,
;;; :arity, and :name. :op is for the function, :arity is the number
;;; of arguments, and :name is the symbol used to print it out if it's
;;; in the answer at the end (you usually want it to be the same as the
;;; name of the function). 
;;;
;;; Here's an example:
;;;
;;;      [{:op *    :arity 2 :name '*}
;;;       {:op +    :arity 2 :name '+}
;;;       {:op sdiv :arity 2 :name '/}
;;;       {:op sin  :arity 1 :name 'sin}]
;;;
;;; For more information on how to use it, see the source code below. The
;;; code is sometimes dense (it's amazing how a few lines of lisp code can
;;; do as much or more as a hundred lines of a more verbose language like
;;; C or Java), but it shouldn't be too hard to understand the general
;;; concepts, especially if you have some familiarity with lisp.

(ns fungp.core
  "This is the start of the core of the library."
  [:use fungp.util])

;;; ### Data structures
;;; 
;;; These are the basic data structures that will be used in the application.

(defrecord TreeAttributes [max-height symbols funcs term depth mutation-rate tournament-size])

(defrecord TestPairs [tests actual])

(defrecord Reporting [repfunc reprate])

(defrecord Tree [code attributes error])

(defrecord Forest [trees best])

(defrecord Population [forests best])

(defn build-options
  "Take passed-in parameters and merge them with default parameters to construct
   the options hash that gets passed to the other functions."
  [o]
  (let [defaults {:depth [1 2] :mutation-rate 0.1 :tournament-size 3
                  :literal-terms [] :term [] :max-height 25}]
    (merge defaults o)))

(defn make-attributes
  "Take the options hash and make a TreeAttributes record."
  [o] (map->TreeAttributes (select-keys o [:max-height :symbols :funcs :term :depth
                                           :mutation-rate :tournament-size])))

(defn terminal
  "Return a random terminal for the source tree. Takes a tree attribute record as a parameter."
  [attr]
  (let [term (:term attr)
        literal-terms (:literal-terms attr)
        symbols (:symbols attr)]
    (if (and (not (empty? term)) (flip 0.5))
      (+ (first term) (rand-int (- (second term) (first term))))
      (rand-nth (concat literal-terms symbols)))))

;;; ### Tree manipulation
;;;
;;; My method of random tree generation is a combination of the "grow" and "fill"
;;; methods of tree building, similar to Koza's "ramped half-and-half" method.

(defn build-tree-code
  "Build a random tree of lisp code. The tree will be filled up to the minimum depth,
   then grown to the maximum depth. Minimum and maximum depth are specified in the
   attribute record, but can optionally be passed explicitly."
  ([attr]
     (build-tree-code attr (second (:depth attr)) (first (:depth attr))))
  ([attr depth-max depth-min]
     (if (or (zero? depth-max)
             (and (<= depth-min 0) (flip 0.5)))
       (terminal attr) ;; insert a random terminal
       (let [f (rand-nth (:funcs attr))]
         (cons (:op f) ;; cons the operation onto a sublist matching the arity of f
               (repeatedly (:arity f) #(build-tree-code
                                        attr (- depth-max 1) (- depth-min 1))))))))

(defn build-tree
  "Build a Tree record from a TreeAttribute record. Optionally takes depth arguments."
  ([attr]
     (Tree. (build-tree-code attr) attr nil))
  ([attr depth-max depth-min]
     (Tree. (build-tree-code attr depth-max depth-min) attr nil)))

;;; First we define a function for creating a collection of trees, then one for
;;; creating a collection of a collection of trees.

(defn build-forest
  "Returns a sequence of trees. A bunch of trees is a forest, right? Get it?"
  [attr forest-size]
  (Forest. (repeatedly forest-size #(build-tree attr)) nil))

(defn build-population
  "Call build-forest repeatedly to fill a population. A population is a collection
   of forests."
  [attr pop-size forest-size]
  (Population. (repeatedly pop-size #(build-forest attr forest-size)) nil))

(defn max-tree-height-code
  "Find the maximum height of a code tree."
  [code]
  (if (not (seq? code)) 0 (+ 1 (reduce max (map max-tree-height-code code)))))

;;; **rand-subtree-code** and **replace-subtree-code** are two of the most important functions.
;;; They define how the lists of code are modified.

;;; The basic idea for how I implemented both of them is that recursion can be
;;; used to reduce the problem at each step: given a tree (or a subtree, all of
;;; which have the same form), recurse on a random subtree, along with a
;;; reduced value of n. The base case is when n is zero or the function hits a leaf.
;;;
;;; Additionally, **replace-subtree** uses concat to reconstruct
;;; the tree on its way back up the stack.
;;;
;;; Both functions expect lists, not Tree records.

(defn rand-subtree-code
  "Return a random subtree of a list."
  ([code]
     (rand-subtree-code code (rand-int (+ 1 (max-tree-height-code code)))))
  ([code n]
     (if (or (zero? n) (not (seq? code))) code
         (recur (rand-nth (rest code))
                (rand-int n)))))

(defn replace-subtree-code
  "Replace a random subtree with a given subtree in a list."
  ([code sub]
     (replace-subtree-code code sub (rand-int (+ 1 (max-tree-height-code code)))))
  ([code sub n]
     (if (or (zero? n) (not (seq? code))) sub
         (let [r (+ 1 (rand-int (count (rest code))))] 
           (concat (take r code)
                   (list (replace-subtree-code
                          (nth code r) sub
                          (rand-int n)))
                   (nthrest code (+ r 1)))))))

(defn truncate
  "Prevent trees from growing too big"
  [tree]
  (if (and (-> tree :attributes :max-height)
           (< (max-tree-height-code (:code tree))
              (-> tree :attributes :max-height)))
    (assoc tree :code (rand-subtree-code (:code tree)))
    tree))

;;; ### Mutation, crossover, and selection
;;;
;;; With rand-subtree and replace-subtree out of the way, the rest of the
;;; single-generation pass is pretty simple. Mutation and crossover both
;;; can easily be written in terms of rand-subtree and replace-subtree.
;;;
;;; **Mutation** takes a Tree record and randomly mutates a part of it.
;;; The idea, like the rest of the fundamental aspects of genetic algorithms,
;;; is taken from nature; when DNA is copied, there is a slight chance of
;;; "mistakes" being introduced in the copy. This can lead to beneficial
;;; changes and increases genetic diversity.

(defn mutate
  "Mutate a tree by substituting in a randomly-built tree of code."
  [tree]
  (truncate (if (flip (-> tree :attributes :mutation-rate))
              (if (flip 0.5)
                (if (or (flip 0.5)
                        (< (max-tree-height-code (:code tree))
                           (-> tree :attributes :max-height)))
                  (assoc tree :code (replace-subtree-code
                                     (:code tree) (build-tree (:attributes tree))))
                  (assoc tree :code (replace-subtree-code
                                     (:code tree) (terminal (:attributes tree)))))
                (assoc tree :code (rand-subtree-code (:code tree)))) ;; subtree lifting
              tree)))

(defn mutate-forest
  "Apply mutate to all trees in a Forest record"
  [forest]
  (assoc forest :trees (map mutate (:trees forest))))

;;; **Crossover** is the process of combining two parents to make a child.
;;; It involves copying the genetic material (in this case, lisp code) from
;;; the two parents, combining them, and returning the result of the combination.

(defn crossover
  "The crossover function is simple to define in terms of replace-subtree
   and rand-subtree. Basically, crossing over two trees involves selecting a
   random subtree from one tree, and placing it randomly in the other tree."
  [tree1 tree2]
  (assoc tree1 :code
         (replace-subtree-code
          (:code tree1)
          (rand-subtree-code (:code tree2)))))

;;; **Selection** is the process in which more fit individuals are "selected," or
;;; more likely to breed (be involved in a crossover), while less fit individuals
;;; are less likely to breed.
;;;
;;; To carry out the selection phase, it's necessary to determine how fit the
;;; individuals are. The following functions use the training data to give the
;;; individual trees a grade, which is the sum of the error. Lower grades are
;;; better. Then, in the selection phase, individuals with lower error are more
;;; likely to be selected for crossover, and thus pass on their genetic
;;; material to the next generation.

(defn find-error
  "Compares the output of the individual tree with the test data to calculate error."
  [tree fit]
  (let [f-symb (list 'fn
                      (-> tree :attributes :symbols)
                      (:code tree))
        f (eval f-symb)]
    (reduce + (map off-by
                   (map #(apply f %) (:tests fit))
                   (:actual fit)))))

(defn forest-error
  "Runs find-error on every tree in a forest, assoc'ing the :error attribute in the record."
  [forest fit]
  (assoc forest :trees (map (fn [tree error]
                              (assoc tree :error error))
                            (:trees forest)
                            (map #(find-error % fit) (:trees forest)))))

(defn get-best
  "Returns the best tree in a forest, according to their error."
  [forest]
  (first (sort-by :error (:trees forest))))

(defn tournament-select-error
  "Select out a few individual trees from the forest and run a
   tournament amongst them. The two most fit in the tournament are crossed over
   to produce a child. Larger tournaments lead to more selective pressure."
  [forest]
  (let [attr (-> forest :trees first :attributes) ;; get attributes from a tree
        tournament (repeatedly (:tournament-size attr) #(rand-nth (:trees forest)))
        selected (sort-by :error tournament)]
    (crossover (first selected) (second selected))))

(defn tournament-select
  "Run tournament-select-error enough times to re-populate the forest."
  [forest]
  (assoc forest :trees
         (repeatedly (count forest)
                     #(tournament-select-error forest))))

;;; ### Putting it together
;;;
;;; This takes care of all the steps necessary to complete one generation of the algorithm.
;;; The process can be extended to multiple generations with a simple tail recursive
;;; function.
;;;
;;; There are some extra considerations here. The function should:
;;;
;;;  * stop when a perfect individual has been found, meaning fitness is zero
;;;  * be resumable, meaning the search can halt, returning information, and that information
;;;    can be passed back in to start the search at the same place

(defn elite
  "Take the best tree so far and put it in the current forest."
  [forest]
  (assoc forest :trees (conj (rest (:trees forest)) (:best forest))))

(defn process-forest-generation
  "Run selection and mutation phases on a Forest record."
  [forest best]
  (let [forest (mutate-forest (tournament-select forest))]
    (elite (assoc forest :best best))))

(defn lower-error
  "Compare records with :error and return the best one."
  [have-error] (first (sort-by :error have-error)))

(defn generations
  "Run n generations of a forest. Over the course of one generation, the trees in
   the forest will go through selection, crossover, and mutation. The best individual
   seen so far in the forest is saved and passed on as the last parameter (it is nil
   when no best individual has been found)."
  [forest fit n]
  (if (or (zero? n)
          (and (not (nil? (-> forest :best :error)))
               (zero? (-> forest :best :error)))) ;; stop early when fitness is zero
    (do (println (-> forest :best))(flush) forest) ;; return this forest
    (let [ferror (forest-error forest fit)
          best (if (nil? (:best forest))
                 (get-best ferror)
                 (lower-error [(get-best ferror) (:best forest)]))
          forest (process-forest-generation ferror best)]
      ;; the recursive call for the next generation
      (do (println n)(flush)(recur forest fit (- n 1))))))

;;; ### Populations
;;;
;;; After building a single tree, then a single generation, then multiple generations,
;;; it's one more step to parallel generations. Above there's a function for defining
;;; a "population" of forests. We can evolve the forests in the population individually
;;; and cross over between them.

(defn forests-crossover
  "Individual trees migrate between forests."
  [forests]
  (let [cross (map rand-nth forests)]
    (map (fn [forest selected]
           (conj (rest (shuffle forest)) selected))
         forests cross)))

(defn population-crossover
  "Cross over the forests in a population."
  [population] (assoc population :forests (forests-crossover (:forests population))))

;;; **parallel-generations** is the function that runs the show. It runs the
;;; generations function defined above on each of the forests (and does so in
;;; parallel, thanks to Clojure's convenient parallelism features).
  
(defn parallel-generations
  "Spawn threads to run each of the forests for a specified amount of time, and
   cross over between the forests at specified intervals. If the search is starting
   from the beginning, the only necessary parameter is the options hash. The initial
   values of the other parameters can be inferred from it. If you're resuming a search
   you can simply pass in the population explicitly and this function will start
   where it left off."
  ([cycles gens pop-size forest-size attr report fit]
     (parallel-generations cycles gens (build-population attr pop-size forest-size) report fit))
  ([cycles gens population report fit]
     (if (or (zero? cycles)
             (and (not (nil? (-> population :best :error)))
                  (zero? (-> population :best :error))))
       (do (println cycles)(println (-> population :best :error))(flush) population)
       (do (when (and (not (nil? (:best population)))
                      (zero? (mod cycles (:reprate report))))
             ((:repfunc report) (:best population))) ;; report
           ;; similar pattern to the generations function
           (let [population (population-crossover
                             (assoc population :forests
                                    (map #(generations % fit gens)
                                          (:forests population))))
                 best (if (nil? (:best population))
                        (get-best (map :best (:forests population)))
                        (lower-error [(get-best (map :best (:forests population)))
                                      (:best population)]))]
             (recur (- cycles 1) gens population report fit))))))

;;; ### Options
;;;
;;; Finally, run-gp exposes a simple API, in the form of a single map parameter. The following options keywords are accepted:
;;;
;;; * *pop-size* --- the number of forests, and the number of top-level threads to run
;;; * *forest-size* --- the number of trees in each forest
;;; * *max-height* --- the maximum height of the tree (larger trees will be shrunk)
;;; * *symbols* --- a sequence of symbols to be placed in the generated code as terminals
;;; * *funcs* --- a sequence (following a certain format; see core.clj or sample.clj) describing the functions to be used in the generated code
;;; * *term* --- a vector representing the range of number terminals to be used in generated code (empty for no number terminals)
;;; * *depth* --- a vector representing the minimum and maximum height of randomly generated trees (defaults to [1 2])
;;; * *repfunc* --- the reporting function, which gets passed the best-seen individual (a hash with keys :tree and :fitness; see sample.clj for an example)
;;; * *reprate* --- the reporting rate; every nth cycle repfunc will be called
;;; * *mutation-rate* --- a number between 0 and 1 that determines the chance of mutation (defaults to 0.05)
;;; * *tournament-size* --- the number of individuals in each tournament selection (defaults to 5)
;;; * *tests* --- test inputs for your function, in the form of a sequence of vectors (each should match the length of *symbols* above)
;;; * *actual* --- the correct outputs for each of the *tests* elements


(defn run-gp
  "Create a population of source trees and evolve them to fit the test function
   and data passed in. This is probably the function you'll want to call."
  [args]
  (let [options (build-options args)]
    (parallel-generations
     (:cycles options)
     (:gens options)
     (:pop-size options)
     (:forest-size options)
     (make-attributes options)
     (:report options)
     (:fit options))))

;;; And that's it! For the core of the library, anyway. 
