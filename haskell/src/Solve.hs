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

distance :: Vertex -> Vertex -> Int
distance (x1, y1) (x2, y2) = truncate . (*100) . sqrt . fromIntegral $ (x1-x2)^2 + (y1-y2)^2

solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
