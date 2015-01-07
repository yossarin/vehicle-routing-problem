module Solve
( solve
, Graph
, Vertex
, Customer
, Warehouse
, Cost
, Demand
, Capacity
, vertexMap
, vertexCost
) where

import Data.Array.Repa hiding (map, (++), zipWith)
import Data.List (sortBy, minimumBy, nub)
import Prelude hiding (lookup)
import Data.Array.Repa.Repr.Vector (fromListVector, V)

type Vertex    = (Int, Int)
type Cost      = Int
type Capacity  = Int
type TruckCap  = Capacity
type TruckCost = Cost
type Demand    = Int
type Customer  = (Vertex, Demand)
type Warehouse = (Vertex, Capacity, Cost)
type Graph     = ([Warehouse], [Customer], TruckCap, TruckCost)

data MapElem = Cus { elemVer :: Vertex
                   , elemDem :: Demand }
             | War { elemVer :: Vertex
                   , elemCap :: Capacity
                   , elemCost :: Cost }
            deriving (Show, Eq)

type IndexVertexMap = Array V DIM1 MapElem
type IndexCostMap   = Array U DIM2 Cost

-- | Route is a list of node indexes (into array) starting from one warehouse
-- | and a travel cost + TruckCost (initial warehouse cost is excluded).
type Route = ([Int], Cost)

-- | Stores single solution data.
data Solution = Solution {
  routes :: [Route], -- ^ Routes of each truck.
  solutionCost :: Cost -- ^ Total cost of the solution.
}

cmpSolutionCost :: Solution -> Solution -> Ordering
cmpSolutionCost a b = solutionCost a `compare` solutionCost b

-- | Returns the lowest cost solution
findBestSolution :: [Solution] -> Solution
findBestSolution = minimumBy cmpSolutionCost

-- | Sorts from lowest cost to highest.
sortSolutions :: [Solution] -> [Solution]
sortSolutions = sortBy cmpSolutionCost

-- | TODO: Needs to take reference to warehouse map.
calcSolutionCost :: [Route] -> TruckCost -> Cost
calcSolutionCost rs tc =
  let traveling = sum $ map snd rs
      trucks = tc * length rs
      whs = usedWarehouses rs
  in undefined

-- | Returns indexes of used warehouses.
usedWarehouses :: [Route] -> [Int]
usedWarehouses = nub . map (head . fst)

euclideanDistance :: Vertex -> Vertex -> Float
euclideanDistance (x1, y1) (x2, y2) =
  sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

travelCost :: Vertex -> Vertex -> Cost
travelCost a = truncate . (*100) . euclideanDistance a

pairwisePermutation :: [a] -> [[a]]
pairwisePermutation [] = []
pairwisePermutation (x:[]) = [[x]]
pairwisePermutation (x:y:[]) = [[x,y],[y,x]]
pairwisePermutation xs = pwP xs (length xs)
  where pwP ys 1 = [last ys : (tail . init $ ys) ++ [head ys], ys]
        pwP ys n = (start ++ b : a : rest) : pwP ys (n-1)
          where (start, a:b:rest) = splitAt (n - 2) ys

cusToElem :: Customer -> MapElem
cusToElem (v, d) = Cus v d

warToElem :: Warehouse -> MapElem
warToElem (v, cap, cost) = War v cap cost

vertexMap :: Graph -> IndexVertexMap
vertexMap (ws, cs, _, _) =
  fromListVector (Z :. len) ((wsEls ++ csEls)::[MapElem])
  where wsEls = map warToElem ws
        csEls = map cusToElem cs
        len  = length ws + length cs

vertexCost :: IndexVertexMap -> IndexCostMap
vertexCost ivm =
  fromListUnboxed (Z :. end :. end) costs
  where costs = zipWith calcCost [0..end-1] [0..end-1]
        calcCost i j = travelCost (getVertex i) (getVertex j)
        getVertex i = elemVer $ ivm ! (Z :. i)
        end = size $ extent ivm


solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
