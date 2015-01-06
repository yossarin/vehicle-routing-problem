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

import Data.Array.Repa hiding (map, (++))
import Prelude hiding (lookup)

type Vertex    = (Int, Int)
type Cost      = Int
type Capacity  = Int
type TruckCap  = Capacity
type TruckCost = Cost
type Demand    = Int
type Customer  = (Vertex, Demand)
type Warehouse = (Vertex, Capacity, Cost)
type Graph     = ([Warehouse], [Customer], TruckCap, TruckCost) 

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

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

vertexMap :: Graph -> Array U DIM1 Vertex
vertexMap (ws, cs, _, _) =
  fromListUnboxed (Z :. size) (wsz ++ csz)
  where wsz = map fst3 ws
        csz = map fst cs
        size = length ws + length cs

vertexCost :: Graph -> Array U DIM2 Cost
vertexCost g =
  fromListUnboxed (Z :. end :. end) costs
  where costs = [travelCost (vm ! (Z :. i)) (vm ! (Z :. j)) | i <- [0..end-1], j <- [0..end-1]]
        vm  = vertexMap g
        end = size $ extent vm

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
