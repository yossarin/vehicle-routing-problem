-- | Defines data types and functions used with it.
module Solve.Data (
  Vertex
, Cost
, Capacity
, TruckCap
, TruckCost
, Demand
, Customer
, Warehouse
, Graph
, Route(..)
, Solution(..)
, cmpSolutionCost
, findBestSolution
, sortSolutions
) where

import Data.List (sortBy, minimumBy)

type Vertex    = (Int, Int)
type Cost      = Int
type Capacity  = Int
type TruckCap  = Capacity
type TruckCost = Cost
type Demand    = Int
type Customer  = (Vertex, Demand)
type Warehouse = (Vertex, Capacity, Cost)
type Graph     = ([Warehouse], [Customer], TruckCap, TruckCost)

-- | Route of a single truck.
data Route = Route {
  routeNodes :: [Int] -- ^ Node indexes (into array) starting from warehouse.
, routeCost :: Cost -- ^ Travel cost + TruckCost (warehouse cost is excluded).
, routeDemand :: Demand -- ^  Total demand for this path.
}

-- | Stores single solution data.
data Solution = Solution {
  routes :: [Route], -- ^ Routes of each truck.
  solutionCost :: Cost -- ^ Total cost of the solution.
}

-- | Compares solutions by their cost.
cmpSolutionCost :: Solution -> Solution -> Ordering
cmpSolutionCost a b = solutionCost a `compare` solutionCost b

-- | Returns the lowest cost solution
findBestSolution :: [Solution] -> Solution
findBestSolution = minimumBy cmpSolutionCost

-- | Sorts from lowest cost to highest.
sortSolutions :: [Solution] -> [Solution]
sortSolutions = sortBy cmpSolutionCost

