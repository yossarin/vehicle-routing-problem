module Solve
( solve
, module Solve.Data
, vertexMap
, vertexCost
) where

import Data.Array.Repa hiding (map, (++), zipWith)
import Data.List (groupBy, sort)
import Data.Array.Repa.Repr.Vector (fromListVector, V)
import Prelude hiding (lookup)
import Solve.Data
import System.Random (RandomGen, randomR)

data MapElem = Cus { elemVer :: Vertex
                   , elemDem :: Demand }
             | War { elemVer :: Vertex
                   , elemCap :: Capacity
                   , elemCost :: Cost }
            deriving (Show, Eq)

type IndexVertexMap = Array V DIM1 MapElem
type IndexCostMap   = Array U DIM2 Cost

-- | Takes a reference to node index data mapping, cost of sending a truck,
-- | routes for each truck and calculates the total cost.
calcSolutionCost :: IndexVertexMap -> [Route] -> Cost
calcSolutionCost ivm rs =
  let traveling = sum $ map routeCost rs
      whs = usedWarehouses rs
      whCosts = sum $ map (\(i,_,_) -> elemCost $ ivm ! (Z :. i)) whs
  in traveling + whCosts

-- | Takes a node index data mapping and a solution. Returns True if all
-- | the routes' demand doesn't exceed the capacity of their warehouses.
canSupply :: IndexVertexMap -> Solution -> Bool
canSupply ivm (Solution rs _) = all canSupply' $ usedWarehouses rs
  where canSupply' (i, _, d) = d <= elemCap (ivm ! (Z :. i))

indexElemVer :: IndexVertexMap -> Int -> Vertex
indexElemVer ivm i = elemVer $ ivm ! (Z :. i)

-- | Takes a node index data mapping, number of warehouses, random number
-- | generator and a route. Returns new route with randomly selected warehouse.
mutateRouteWarehouse :: RandomGen g =>
  IndexVertexMap -> Int -> g -> Route -> (Route, g)
mutateRouteWarehouse ivm whs rg r =
  let (rnd, rg') = randomR (0, whs - 1) rg
  in (changeWarehouse ivm rnd r, rg')

-- | Takes node index data mapping, new warehouse index and a route.
-- | Constructs a new route with replaced warehouse node index.
changeWarehouse :: IndexVertexMap -> Int -> Route -> Route
changeWarehouse ivm w r@(Route ns rc _) =
  let oldWh = indexElemVer ivm $ head ns
      newWh = indexElemVer ivm w
      start = indexElemVer ivm . head $ tail ns
      end   = indexElemVer ivm $ last ns
      oldWhTravel = travelCost oldWh start + travelCost oldWh end
      newWhTravel = travelCost newWh start + travelCost newWh end
  in r { routeNodes = w : tail ns, routeCost = rc - oldWhTravel + newWhTravel }

-- | Returns indexes of used warehouses, counts how many times they are used and
-- | the total demand for capacity of that warehouse.
usedWarehouses :: [Route] -> [(Int, Int, Demand)]
usedWarehouses = map cnts . groupBy (\(a,_) (b,_) -> a == b) . sort . map whDem
  where whDem r = (head $ routeNodes r, routeDemand r)
        cnts xs = (fst $ head xs, length xs, sum $ map snd xs)

euclideanDistance :: Vertex -> Vertex -> Float
euclideanDistance (x1, y1) (x2, y2) =
  sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

travelCost :: Vertex -> Vertex -> Cost
travelCost a = truncate . (*100) . euclideanDistance a

-- | Takes a list and returns a list of pairwise permutations (2-opt exchanges).
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

-- | Takes a graph of the problem and creates a 1 dimensional array
-- | which maps index ordered nodes to their data (capacity, vertex etc.).
vertexMap :: Graph -> IndexVertexMap
vertexMap (ws, cs, _, _) =
  fromListVector (Z :. len) ((wsEls ++ csEls)::[MapElem])
  where wsEls = map warToElem ws
        csEls = map cusToElem cs
        len  = length ws + length cs

-- | Takes a mapping of node indexes to their vertices and creates index based
-- | matrix of travel costs between nodes.
vertexCost :: IndexVertexMap -> IndexCostMap
vertexCost ivm =
  fromListUnboxed (Z :. end :. end) costs
  where costs = zipWith calcCost [0..end-1] [0..end-1]
        calcCost i j = travelCost (getVertex i) (getVertex j)
        getVertex i = indexElemVer ivm i
        end = size $ extent ivm

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
