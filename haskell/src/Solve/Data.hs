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
, ProbabilityFun
, Graph
, Route(..)
, Solution(..)
, cmpSolutionCost
, findBestSolution
, sortSolutions
, solutionToString
, probabilitySelect
, rouletteWheel
) where

import Data.List (sortBy, minimumBy, intercalate)
import System.Random (RandomGen, randomR)

type Vertex    = (Int, Int)
type Cost      = Int
type Capacity  = Int
type TruckCap  = Capacity
type TruckCost = Cost
type Demand    = Int
type Customer  = (Vertex, Demand)
type Warehouse = (Vertex, Capacity, Cost)
type Graph     = ([Warehouse], [Customer], TruckCap, TruckCost)

type ProbabilityFun a = a -> Double

-- | Takes a function which maps elements with their probabilites, list of
-- | elements and a random generator. Returns roulette wheel selected element.
probabilitySelect :: RandomGen g => ProbabilityFun a -> [a] -> g -> (Maybe a, g)
probabilitySelect prob xs rg = rouletteWheel rg xs' sumProb
  where (xs', sumProb) = mapProb xs ([], 0.0)
        mapProb [] res = res
        mapProb (y:ys) (zs, sp) = mapProb ys ((y, p):zs, sp + p)
          where p = prob y

-- | Takes random generator, list of elements paired with their probabilites,
-- | sum of all probabilities and returns one element and new generator.
-- | If the list is empty returns Nothing as selected element.
rouletteWheel :: RandomGen g => g -> [(a, Double)] -> Double -> (Maybe a, g)
rouletteWheel rg xs maxProb = (select 0.0 sorted, rg')
  where (rnd, rg') = randomR (0.0, maxProb) rg
        sorted = sortBy (\(_,px) (_,py) -> py `compare` px) xs
        select _ [] = Nothing
        select acc ((x, p):ys) | rnd <= p + acc = Just x
                               | otherwise = select (p + acc) ys

-- | Route of a single truck.
data Route = Route {
  routeNodes :: [Int] -- ^ Node indexes (into array) starting from warehouse.
, routeCost :: Cost -- ^ Travel cost + TruckCost (warehouse cost is excluded).
, routeDemand :: Demand -- ^  Total demand for this path.
} deriving (Show)

-- | Stores single solution data.
data Solution = Solution {
  routes :: [Route], -- ^ Routes of each truck.
  solutionCost :: Cost -- ^ Total cost of the solution.
} deriving (Show)

-- | Compares solutions by their cost.
{-# INLINE cmpSolutionCost #-}
cmpSolutionCost :: Solution -> Solution -> Ordering
cmpSolutionCost a b = solutionCost a `compare` solutionCost b

-- | Returns the lowest cost solution
findBestSolution :: [Solution] -> Solution
findBestSolution = minimumBy cmpSolutionCost

-- | Sorts from lowest cost to highest.
sortSolutions :: [Solution] -> [Solution]
sortSolutions = sortBy cmpSolutionCost

-- | Returns String representation of Route, takes node index offset for
-- | non warehouse nodes.
routeToString :: Int -> Route -> String
routeToString offset r = show w ++ ":  " ++ nodes
  where (w:ns) = routeNodes r
        nodes = unwords $ map (show . (+offset)) ns

-- | Returns String representation of Solution, takes node index offset
-- | for non warehouse nodes.
solutionToString :: Int -> Solution -> String
solutionToString o s = n ++ lf ++ intercalate lf rs ++ lf ++ "\n" ++ sc
  where rs = map (routeToString o) $ routes s
        sc = show $ solutionCost s
        n  = show $ length rs
        lf = "\n\n"
