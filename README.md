Multiple depot vehicle routing problem
======================================

This source code represents solution for multiple depot vehicle routing problem. [Ant Colony Optimization](https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms) algorithm is used. This solution was developed as an assignment for [Heuristic Optimization Methods class](http://www.fer.unizg.hr/en/course/hom) at [Faculty of Electrical Engineering and Computing](https://www.fer.unizg.hr/en), University of Zagreb.

This implementation uses [Repa](https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial) library that makes the code automatically concurrent. This opens up the possibility od utilizing as many cores of the processor as one likes, provided one supplies `+RTS -N` on the command line when running the program.

Authors: Teon Banek, Vlatko Klabučar, Damir Tomić

### Ant Colony Optimization pseudocode for solving multiple depot vehicle routing problem.

Adjustable parameters are in stars, e.g. `*alpha*`.

```
repeat max-iterations:
    # Generate *m* feasible solutions.
    repeat *m*:
        solutions += generate-solution(*alpha*, *beta*, pheromones)
    # Generate new solutions from existing by mutating each with *mut-prob*.
    mutations = mutate-solutions(*mut-prob*, solutions)
    # Find best among all current solutions.
    current-best = find-best(mutations, solutions)
    # Update pheromones according to current best solution.
    update-pheromones(*decay-rate*, *deposit-rate*, pheromones, current-best)
    # Set all time best
    if current-best < best:
        best = current-best

fn generate-solution(*alpha*, *beta*, pheromones):
    until visited-all(visited)
      # Start new route if current truck has unsufficient cargo.
      next-city = probability-select(*alpha*, *beta*, pheromones)
      visited += next-city
    return best-2-opt-exchange(visited)
```