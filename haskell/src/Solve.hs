module Solve
( solve
, Graph
, Vertex
, Customer
, Warehouse
, Cost
, Demand
, Capacity
) where

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

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
