module Solve
( solve
, module Solve.Data
, vertexMap
, vertexCost
) where

import Data.Array.Repa hiding (map, (++), zipWith)
import Data.List (nub)
import Data.Array.Repa.Repr.Vector (fromListVector, V)
import Prelude hiding (lookup)
import Solve.Data

data MapElem = Cus { elemVer :: Vertex
                   , elemDem :: Demand }
             | War { elemVer :: Vertex
                   , elemCap :: Capacity
                   , elemCost :: Cost }
            deriving (Show, Eq)

type IndexVertexMap = Array V DIM1 MapElem
type IndexCostMap   = Array U DIM2 Cost

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
-- | which maps index ordered nodes to their coordinate vertices.
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
        getVertex i = elemVer $ ivm ! (Z :. i)
        end = size $ extent ivm

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
