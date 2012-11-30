fungp-moses
=====

This is the README for **fungp-moses**, an extension of the genetic programming library written by [Mike Vollmer](http://recurial.com/projects/fungp/) [(Github)] (http://github.com/probabilityZero/fungp).

This extension is written by [Chris Liberatore](http://www.cliberatore.com/blog) as a term project for the Fall 2012 semester's holding of CSC 215, Artificial Intelligence at [CSU Sacramento](http://csus.edu). CSC 215 is taught by [Dr. Scott Gordon](http://gaia.ecs.csus.edu/~gordonvs/).

About this program
------------------

**fungp** is a genetic programming library implemented in the Clojure programming language. It's pronounced
fun-gee-pee, for *functional genetic programming*.

**fungp-moses** is an extension of **fungp**, in which aspects of the [Meta-Optimizing Semantic Evolutionary Search (MOSES) algorithm] (http://wiki.opencog.org/w/Meta-Optimizing_Semantic_Evolutionary_Search), are implemented. Mosheed Looks described this in [his GECCO '07 article] (http://dl.acm.org/citation.cfm?id=1277086) (behind a paywall, if you're a student somewhere, go through your library and get access to the [ACM Digital Library](dl.acm.org))

MOSES is so named because of the two additional operations he added to Genetic Programming: a semantic normalization operation, and a [estimation-of-distribution algorithm](http://en.wikipedia.org/wiki/Estimation_of_distribution_algorithm) optimization operator. The **normalization** operator seeks to maintain the both semantic meaning of a given candidate program as well as much of the structure of the program as possible (as to maintain genetic diversity). The **optimization** operator uses a particular implementation of an estimation-of-distribution algorithm known as Bayesian Optimization (which utilizes both [Bayesian Networks](http://en.wikipedia.org/wiki/Bayesian_network) as well as an *eod* algorithm) to optimize a subset of the terminals of candidate programs, such that the program is the most-fit it can possibly be for its structure.

Currently, the normalization operation relies on the clojure [core.match](https://github.com/clojure/core.match) library and [destructuring](http://clojure.org/special_forms#Special%20Forms--Binding%20Forms%20(Destructuring)) to navigate and normalize trees.

While it will be the goal to implement a Bayesian Optimization operation in this implementation of MOSES, this iteration seeks to implement a far more simple iteration at the outset. Estimation-of-distributions are simple statistically-driven directed search, similar to [hill-climbing algorithms](http://en.wikipedia.org/wiki/Hill_climbing). A hill-climbing search will be used in place of Bayesian Optimization for the initial iterations of this project.

Dependencies
-------------------
Like the original project, this project requires that you use [Leiningen](https://github.com/technomancy/leiningen) as your project management tool.

Additionally, this project relies on the clojure [core.match](https://github.com/clojure/core.match) library, as normalization is done via pattern-matching in candidate trees. Please refer to the link for all pertinent documentation, but **core.match** provides ML-like pattern-matching for Clojure.

How do I set it up?
-------------------
I would direct you to the original [fungp README, here on github](http://github.com/probabilityZero/fungp) for proper instructions as to how to run this application.

Additions to FunGP
-------------------
 * A basic normalization framework which utilizes dynamic bindings, offering "hooks" for an end-user to inject custom normalization operations
 * A very basic regression problem frameworked using normalization
 * An implementation of the "Two Box Problem" taken from [Genetic Programming II, by John Koza](http://www.amazon.com/Genetic-Programming-II-Automatic-Discovery/dp/0262111896/)

Goals of FunGP-MOSES
--------------------

Links and References
--------------------

 * [Koza's GP Website](http://www.genetic-programming.org/)
 * [Dr. Gordon's Website at CSUS](http://gaia.ecs.csus.edu/~gordonvs/)
 * [A Field Guide to Genetic Programming](http://www.gp-field-guide.org.uk/)

License
-------

Project created by Mike Vollmer and released under GPL.

MOSES additions were created by Chris Liberatore and are also released under GPL.

See the LICENSE file distributed with this code.
