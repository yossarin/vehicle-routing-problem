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

import Data.Array
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
pairwisePermutation []       = []
pairwisePermutation (x:[])   = [[x]]
pairwisePermutation (x:y:[]) = [[x,y],[y,x]]
pairwisePermutation xs       = pwP xs (length xs)
  where pwP ys 1 = [[last ys] ++ (tail . init $ ys) ++ [head ys], ys]
        pwP ys n = ((take (n-2) ys) ++ [ys!!(n-1)] ++ [ys!!(n-2)] ++ (drop n ys)):(pwP ys (n-1))

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

vertexMap :: Graph -> Array Int Vertex
vertexMap (ws, cs, _, _) = array (1, (length ws)+(length cs)) (wsz ++ csz)
  where wsz = zip [1..] (map fst3 ws)
        csz = zip [((length ws)+1)..] (map fst cs)

vertexCost :: Graph -> Array (Int, Int) Cost
vertexCost g = array ((1,1), (end, end)) [((i,j), travelCost (vm!i) (vm!j)) | i <- [1..end], j <- [1..end]]
  where vm  = vertexMap g
        end = snd $ bounds vm

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
