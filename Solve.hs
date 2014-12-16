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
type Graph     = ([Warehouse], [User], TruckCap, TruckCost) 
