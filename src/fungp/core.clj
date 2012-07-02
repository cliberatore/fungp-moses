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
;;;  * Options for tree leaves: number literals, symbols, or quoted Lisp code
;;;  * Data structures compiled to Java classes
;;;
;;; How do I use it?
;;; ----------------
;;;
;;; Call the **run-gp** function. See the source code for the full list of 
;;; keyword parameters.
;;;
;;; Here's an example, taken from sample.clj:
;;;
;;;      (def results (run-gp {:gens iter :cycles cycle :term [-1 1]
;;;                            :pop-size 12 :forest-size 250 :depth [2 3]
;;;                            :symbols symbols :funcs funcs
;;;                            :report {:repfunc repfunc  :reprate 1}
;;;                            :tournament-size 5
;;;                            :fit {:actual actual :tests testdata}}))
;;;
;;; Functions are defined as a sequence of maps, each having keys :op
;;; and :arity.
;;;
;;; Here's an example:
;;;
;;;      [{:op *    :arity 2}
;;;       {:op +    :arity 2}
;;;       {:op sdiv :arity 2}
;;;       {:op sin  :arity 1}]
;;;
;;; For more information on how to use it, see the source code below. The
;;; code is sometimes dense (it's amazing how a few lines of lisp code can
;;; do as much or more as a hundred lines of a more verbose language like
;;; C or Java), but it shouldn't be too hard to understand the general
;;; concepts, especially if you have some familiarity with lisp.

(ns fungp.core
  "This is the start of the core of the library. Include the fungp.core namespace
   if you want to use run-gp."
  [:use fungp.util])

;;; ### Data structures
;;; 
;;; These are the basic data structures that will be used in the application. Most
;;; of them should be self-explanatory, except TreeAttributes -- it holds any
;;; information needed to manipulate trees. A minor complication is the inclusion
;;; of tournament-size, since it acts on forests rather than trees, so it picks
;;; a tree from the forest to read tournament-size from.

(defrecord PrimitiveProcedure
    [op arity])

(defrecord TreeAttributes
    [max-height symbols funcs term depth mutation-rate tournament-size])

(defrecord TestPairs
    [tests actual])

(defrecord Reporting
    [repfunc reprate])

(defrecord Tree
    [code attributes error])

(defrecord Forest
    [trees best])

(defrecord Population
    [forests best])

;;; The main entry function takes a hash as a parameter. Before using it I merge it
;;; with a hash of default values.

(defn build-options
  "Take passed-in parameters and merge them with default parameters to construct
   the options hash that gets passed to the other functions."
  [o]
  (let [defaults {:depth [1 2] :mutation-rate 0.1 :tournament-size 3
                  :leaves [] :term [] :max-height 25}]
    (merge defaults o)))

;;; Any data needed to manipulate trees is stored in a TreeAttributes record. One
;;; search will share a single TreeAttribute record. 

(defn make-attributes
  "Take the options hash and make a TreeAttributes record."
  [o] (map->TreeAttributes (select-keys o [:max-height :symbols :funcs :term :depth
                                           :mutation-rate :tournament-size])))

;;; ### Tree manipulation
;;;
;;; The following section of code deals with building and manipulating the trees of code,
;;; which are actually quoted lists of Lisp code. The goal is to make sure valid code is
;;; generated to start with, and that all operations (crossover, mutation, etc) output
;;; valid code. If those two conditions are true, the algorithm will produce valid code at
;;; the end of each iteration (generation).
;;;
;;; To create the leaves of the tree I either use number literals (between a specified range),
;;; parameter symbols (which correspond to the input of the function), or "literal-terms," which
;;; can be any quoted Lisp code, from constants like Math/PI to side-effect code like file I/O.

(defn terminal
  "Return a random terminal for the source tree. Takes a TreeAttribute record as a parameter.
   Terminals are pulled from :term (for number literals), :symbols (for parameters), and
   :literal-terms (for everything else) -- each read from the TreeAttribute record."
  [attr]
  (let [term (:term attr)
        literal-terms (:leaves attr)
        symbols (:symbols attr)]
    (if (and (not (empty? term)) (flip 0.5))
      (+ (first term) (rand-int (- (second term) (first term))))
      (rand-nth (concat literal-terms symbols)))))

;;; My method of random tree generation is a combination of the "grow" and "fill"
;;; methods of tree building, similar to Koza's "ramped half-and-half" method.
;;;
;;; Here is the first instance of a pattern I use occasionally: if I end a function with
;;; "-code" it means it operates on the list of code itself, rather than a Tree record.

(defn build-tree-code
  "Designed to be called by build-tree.
   Build a random tree of lisp code. The tree will be filled up to the minimum depth,
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
     (Tree. (build-tree-code attr) attr Integer/MAX_VALUE))
  ([attr depth-max depth-min]
     (Tree. (build-tree-code attr depth-max depth-min) attr Integer/MAX_VALUE)))

;;; First we define a function for creating a collection of trees, then one for
;;; creating a collection of a collection of trees.

(defn build-forest
  "Returns a Forest record. A bunch of trees is a forest, right? Get it?"
  [attr forest-size]
  (let [trees (repeatedly forest-size #(build-tree attr))
        best (rand-nth trees)]
    (Forest. trees best)))

(defn build-population
  "Call build-forest repeatedly to fill a population. A population is a collection
   of forests."
  [attr pop-size forest-size]
  (let [population (repeatedly pop-size #(build-forest attr forest-size))
        best (-> population rand-nth :trees rand-nth)]
    (Population. population best)))

(defn max-tree-height-code
  "Find the maximum height of a code tree."
  [code]
  (if (not (seq? code)) 0
      (+ 1 (reduce max
                   (map max-tree-height-code code)))))

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

;;; One minor problem with the method of evaluation I'm using is that there's a limit to
;;; how big the generated functions can be. Since they're compiled into JVM bytecode,
;;; they are subject to the JVM's limit on how big methods can be. This is a huge limit
;;; but because it technically exists I make a maximum height mandatory for generated
;;; trees.

(defn truncate
  "Prevent trees from growing too big by doing subtree lifting when they hit the cap."
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
  "Mutate a tree by substituting in a randomly-built tree of code. This function
   also calls truncate. There are three types of mutation supported: replacing a
   random subtree with a new random tree, replacing a random subtree with a
   random terminal, and lifting a random subtree to the root."
  [tree]
  (truncate (if (flip (-> tree :attributes :mutation-rate))
              (if (flip 0.5)
                (if (or (flip 0.5)
                        (< (max-tree-height-code (:code tree))
                           (-> tree :attributes :max-height)))
                  ;; assoc the code into the Tree record
                  (assoc tree :code (replace-subtree-code
                                     (:code tree) (build-tree-code (:attributes tree))))
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
  "Compares the output of the individual tree with the test data to calculate error.
   This is where the generated trees are compiled and executed."
  [tree fit]
  (let [f-symb (list 'fn ;; format the quoted list to be eval'd
                      (-> tree :attributes :symbols)
                      (:code tree))
        f (eval f-symb)] ;; compile the generated function
    (reduce + (map (fn [x y] (if (> x y) (- x y) (- y x)))
                   (map #(apply f %) (:tests fit))
                   (:actual fit)))))

(defn tree-error
  "Calls find-error, takes a Tree record."
  [tree fit]
  (assoc tree :error (find-error tree fit)))

(defn forest-error
  "Runs tree-error on every tree in a forest, assoc'ing the :error attribute in the record."
  [forest fit]
  (assoc forest :trees (map #(tree-error % fit) (:trees forest))))

(defn get-best
  "Returns the best tree in a forest, according to their error."
  [forest]
  (first (sort-by :error (:trees forest))))

(defn tournament-select-error
  "Designed to be called by tournament-select.
   Select out a few individual trees from the forest and run a
   tournament amongst them. The two most fit in the tournament are crossed over
   to produce a child. Larger tournaments lead to more selective pressure."
  [forest]
  (let [attr (-> forest :trees first :attributes) ;; get attributes from some tree
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
;;; The next thing to do is combine the operations. One generation consists
;;; of applying the mutation and selection phases on a forest of trees.
;;;
;;; 

(defn forest-best-update
  "Update the :best attribute of the Forest record."
  [forest]
  (let [current-best (get-best forest)
        old-best (-> forest :best)]
    (if (or (nil? old-best)
            (< (:error current-best) (:error old-best)))
      (assoc forest :best current-best)
      forest)))

(defn elite
  "Take the best tree so far and put it in the current forest."
  [forest]
  (assoc forest :trees (conj (rest (:trees forest)) (:best forest))))

;;; This takes care of all the steps necessary to complete one generation of the algorithm.
;;; The process can be extended to multiple generations with a simple tail recursive
;;; function.
;;;
;;; There are some extra considerations here. The generation function should:
;;;
;;;  * stop when a perfect individual has been found, meaning fitness is zero
;;;  * be resumable, meaning the search can halt, returning information, and that information
;;;    can be passed back in to start the search at the same place

(defn generations
  "Run n generations of a forest. Over the course of one generation, the trees in
   the forest will go through selection, crossover, and mutation. The best individual
   seen so far in the forest is saved and passed on as the last parameter (it is nil
   when no best individual has been found)."
  [forest fit n]
  (if (or (zero? n)
          (zero? (-> forest :best :error)))
    forest ;; return current forest
    (let [forest (-> forest (forest-error ,,, fit) ;; calculate error before
                     tournament-select mutate-forest
                     (forest-error ,,, fit) forest-best-update)] ;; and after
      ;; the tail-recursive call for the next generation
      (recur forest fit (- n 1)))))

;;; ### Populations
;;;
;;; After building a single tree, then a single generation, then multiple generations,
;;; it's one more step to parallel generations. Above there's a function for defining
;;; a "population" of forests. We can evolve the forests in the population individually
;;; and cross over between them.

(defn population-crossover
  "Migrate individual trees between populations by making a list of individuals pulled
   randomly and merging it back into the population in a different order."
  [population]
  (let [cross-list (map #(-> % :trees rand-nth) (shuffle (-> population :forests)))]
    (assoc population :forests
           (map (fn [forest tree]
                  (if (not (in? (:trees forest) tree))
                    (assoc forest :trees (conj (-> forest :trees shuffle rest) tree))
                    forest))
                (:forests population) cross-list))))

(defn population-forest-update
  "Update the :forest attribute of a Population by calling generation and
   calling population-crossover"
  [population fit gens]
  (population-crossover (assoc population :forests
                               (map #(generations % fit gens)
                                    (:forests population)))))

(defn population-best-update
  "Update the :best attribute of a Population by finding the lowest error
   among the :best attributes of each forest."
  [population]
  (assoc population :best
         (first (sort-by :error
                         (map :best
                              (:forests population))))))

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
     ;; no population generated yet
     (parallel-generations cycles gens (build-population attr pop-size forest-size) report fit))
  ([cycles gens population report fit]
     (if (or (zero? cycles)
             (zero? (-> population :best :error)))
       population
       (do (when (and (not (nil? (:best population)))
                      (zero? (mod cycles (:reprate report))))
             (when (not= (-> population :best :error) Integer/MAX_VALUE)
               ((:repfunc report) (:best population)))) ;; report
           (let [population (-> population
                                (population-forest-update ,,, fit gens)
                                population-best-update)]
             (recur (- cycles 1) gens population report fit))))))

;;; ### Options
;;;
;;; Finally, run-gp exposes a simple API, in the form of a single map parameter. The following options keywords are accepted:
;;;
;;; * *:pop-size* --- the number of forests, and the number of top-level threads to run
;;; * *:forest-size* --- the number of trees in each forest
;;; * *:max-height* --- the maximum height of the tree (larger trees will be shrunk)
;;; * *:symbols* --- a sequence of symbols to be placed in the generated code as terminals
;;; * *:funcs* --- a sequence (following a certain format; see core.clj or sample.clj) describing the functions to be used in the generated code
;;; * *:term* --- a vector representing the range of number terminals to be used in generated code (empty for no number terminals)
;;; * *:depth* --- a vector representing the minimum and maximum height of randomly generated trees (defaults to [1 2])
;;; * *:report
;;;     - *:reprate* --- the reporting function, which gets passed the best-seen individual (a hash with keys :tree and :fitness; see sample.clj for an example)
;;;     - *:reprate* --- the reporting rate; every nth cycle repfunc will be called
;;; * *:mutation-rate* --- a number between 0 and 1 that determines the chance of mutation (defaults to 0.05)
;;; * *:tournament-size* --- the number of individuals in each tournament selection (defaults to 5)
;;; * *:fit*
;;;     - *:tests* --- test inputs for your function, in the form of a sequence of vectors (each should match the length of *symbols* above)
;;;     - *:actual* --- the correct outputs for each of the *tests* elements


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
