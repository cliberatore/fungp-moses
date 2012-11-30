(ns fungp.moses.test)

(use 'fungp.core 'fungp.moses 'fungp.tutorial 'fungp.moses.normalize 'criterium.core)

(defn run-gp
  "This is the function that launches *fungp* and starts the evolution.
   This is used in the benchmarking of the FunGP algorithm with various parameters.
   Options include the ability to determine if and what to use for normalization and
   optimization. The function returns the exemplar of the population.
   This function is intended to be used with the Criterium benchmarking tool."
  [iterations migrations terminals literals functions fitness options]
  (let [gp-options {:iterations iterations :migrations migrations
                 :num-islands 6 :population-size 40
                 :tournament-size 5 :mutation-probability 0.1
                 :max-depth 10 :terminals terminals
                 :numbers literals :fitness fitness
                 :functions functions :report sample-report }
        ;; the data returned by run-genetic-programming is as follows:
        ;; [population [best-tree score]]
        ;; since we don't care about keeping the whole population
        ;; around, we can save off the tree and score like this
        [tree score] (rest (run-fungp-moses gp-options))]
    [tree score]))

(def std-options {:iterations 15 :migrations 15
                  :terminals sample-parameters :max-depth 10
                  :numbers number-literals :fitness sample-fitness
                  :functions sample-functions :report sample-report
                  :num-islands 1 :population-size 10})