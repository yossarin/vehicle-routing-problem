{-# LANGUAGE BangPatterns #-}

module Solve
( solve
, module Solve.Data
, ACOParameters
) where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Array.Repa hiding (map, (++), zipWith)
import qualified Data.Array.Repa.Operators.Mapping as M
import Data.Array.Repa.Unsafe (unsafeTraverse)
import Data.Array.Repa.Repr.Vector (fromListVector, V)
import Data.List (delete, foldl', groupBy, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Prelude hiding (lookup)
import Solve.Data
import System.Random (getStdGen, mkStdGen, RandomGen, randomR, randoms, StdGen)

data MapElem = Cus { elemVer :: Vertex
                   , elemDem :: Demand }
             | War { elemVer :: Vertex
                   , elemCap :: Capacity
                   , elemCost :: Cost }
            deriving (Show, Eq)

type IndexVertexMap = Array V DIM1 MapElem
type IndexCostMap   = Array U DIM2 Cost
type PheromoneMap   = Array U DIM2 Double

-- | Takes a reference to node index data mapping, cost of sending a truck,
-- | routes for each truck and calculates the total cost.
{-# INLINE calcSolutionCost #-}
calcSolutionCost :: IndexVertexMap -> [Route] -> Cost
calcSolutionCost ivm rs =
  let traveling = sum $ map routeCost rs
      whs = usedWarehouses rs
      whCosts = sum $ map (\(i,_,_) -> elemCost $ ivm `unsafeIndex` (Z :. i)) whs
  in traveling + whCosts

-- | Takes a node index travel cost mapping, list of nodes and calculates
-- | total travel cost with returning to starting location.
{-# INLINE calcTravelCost #-}
calcTravelCost :: IndexCostMap -> [Int] -> Cost
calcTravelCost icm xs = sum $ map calcCost pairs
  where pairs = zip xs $ tail $ cycle xs
        calcCost (i,j) = icm `unsafeIndex` (Z :. i :. j)

-- | Takes a node index data mapping and a solution. Returns True if all
-- | the routes' demand don't exceed the capacity of their warehouses.
canSupply :: IndexVertexMap -> Solution -> Bool
canSupply ivm (Solution rs _) = all canSupply' $ usedWarehouses rs
  where canSupply' (i, _, d) = d <= elemCap (ivm `unsafeIndex` (Z :. i))

{-# INLINE indexElemVer #-}
indexElemVer :: IndexVertexMap -> Int -> Vertex
indexElemVer ivm i = elemVer $ ivm `unsafeIndex` (Z :. i)

-- | Takes node index data and index cost mappings, number of warehouses,
-- | mutation probability, random generator and a solution. Returns new
-- | solution which may have been mutated and if it can supply its routes.
mutateSolution :: RandomGen g =>
  IndexVertexMap -> IndexCostMap -> TruckCost ->
  Int -> Float -> g -> Solution -> (Solution, g)
mutateSolution ivm icm tc whs prob rg s@(Solution rs _) =
  let (rs', rg') = mutateRoutes icm tc whs prob rg rs
      sc = calcSolutionCost ivm rs'
      s' = Solution rs' sc
  in if canSupply ivm s' then (s', rg') else (s, rg')

-- | Takes a node index cost mapping, number of warehouses, mutation
-- | mutation probability, random number generator and a list of routes.
-- | Returns new list of routes, of which some may have different warehouses.
mutateRoutes :: RandomGen g =>
  IndexCostMap -> TruckCost ->
  Int -> Float -> g -> [Route] -> ([Route], g)
mutateRoutes icm tc whs prob rg = foldl' mutate ([], rg)
  where mutate (xs, g) x = (fromMaybe x x' : xs, g')
          where (x', g') = mutateRouteWarehouse icm tc whs prob g x

-- | Takes a node index cost mapping, number of warehouses, mutation
-- | probability, random number generator and a route. Returns new route if
-- | mutation happens, otherwise Nothing.
mutateRouteWarehouse :: RandomGen g =>
  IndexCostMap -> TruckCost ->
  Int -> Float -> g -> Route -> (Maybe Route, g)
mutateRouteWarehouse icm tc whs prob rg r =
  let (rnd, rg') = randomR (0.0, 1.0) rg
      (newRoute, rg'') = randomRouteWarehouse icm whs rg' r
  in if prob > rnd then (Nothing, rg')
    else (Just $ bestRoutePairwisePermutation icm tc newRoute, rg'')

-- | Takes a node index cost mapping, number of warehouses, random number
-- | generator and a route. Returns new route with randomly selected warehouse.
randomRouteWarehouse :: RandomGen g =>
  IndexCostMap ->
  Int -> g -> Route -> (Route, g)
randomRouteWarehouse icm whs rg r =
  let (rnd, rg') = randomR (0, whs - 1) rg
  in (changeWarehouse icm rnd r, rg')

-- | Takes node index cost mapping, new warehouse index and a route.
-- | Constructs a new route with replaced warehouse node index.
changeWarehouse :: IndexCostMap -> Int -> Route -> Route
changeWarehouse icm w r@(Route ns rc _) =
  let oldWh = head ns
      start = head $ tail ns
      end   = last ns
      oldWhTravel = icm `unsafeIndex` (Z :. oldWh :. start) + icm `unsafeIndex` (Z :. oldWh :. end)
      newWhTravel = icm `unsafeIndex` (Z :. w :. start) + icm `unsafeIndex` (Z :. w :. end)
  in r { routeNodes = w : tail ns, routeCost = rc - oldWhTravel + newWhTravel }

-- | Returns indexes of used warehouses, counts how many times they are used and
-- | the total demand for capacity of that warehouse.
usedWarehouses :: [Route] -> [(Int, Int, Demand)]
usedWarehouses = map cnts . groupBy (\(a,_) (b,_) -> a == b) . sort . map whDem
  where whDem r = (head $ routeNodes r, routeDemand r)
        cnts xs = (fst $ head xs, length xs, sum $ map snd xs)

{-# INLINE euclideanDistance #-}
euclideanDistance :: Vertex -> Vertex -> Float
euclideanDistance (x1, y1) (x2, y2) =
  sqrt . fromIntegral $ (x1 - x2)^2 + (y1 - y2)^2

{-# INLINE travelCost #-}
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
  where costs = [calcCost i j | i <- [0..end-1], j <- [0..end-1]]
        calcCost i j = travelCost (getVertex i) (getVertex j)
        getVertex = indexElemVer ivm
        end = size $ extent ivm

-- | Takes a mapping of node indices to their vertices and creates index based
-- | matrix of pheromone trails with initial value p
initPheromoneMap :: IndexVertexMap -> Double -> PheromoneMap
initPheromoneMap ivm p = 
  fromListUnboxed (Z :. end :. end) (replicate (end*end) p)
  where end = size $ extent ivm

-- | Takes a mapping of pheromones and decreases its values by fixed rate r
{-# DEPRECATED evaporatePheromoneMap "Use updatePheromoneMap" #-}
evaporatePheromoneMap :: Monad m => PheromoneMap -> Double -> m PheromoneMap
evaporatePheromoneMap ivm r = computeP $ M.map (\x -> (1-r)*x) ivm

-- | Takes Solution, PheromoneMap and parameters for adjusting the reinfocement
-- | and evaporation, then reinforces and evaporates the PheromoneMap according
-- | to the standard reinforcement and evaporation formula for ACO.
updatePheromoneMap :: Monad m =>
  Solution -> PheromoneMap -> Double -> Double -> m PheromoneMap
updatePheromoneMap !s !pm !scal !r = computeP $ unsafeTraverse pm id update 
  where !delta    = scal / fromIntegral (solutionCost s)
        !edges    = concatMap (pairs . routeNodes) $ routes s
        pairs [] = []
        pairs xs@(x:_) = (x,x) : zip xs (tail $ cycle xs)
        update get (Z :. i :. j) = 
          if (i,j) `elem` edges then 
            get (Z :. i :. j)*(1-r) + delta
          else
            get (Z :. i :. j)*(1-r)

-- | Takes initial position, potential, PheromoneMap, IndexCostMap,
-- | parameter a and b and calculates the probability of choosing
-- | the potential position as a next hop via standard formula for ACO.
-- | The probability isn't scaled to [0, 1] interval.
{-# INLINE probability #-}
probability :: Int -> Int 
  -> PheromoneMap -> IndexCostMap
  -> Double -> Double -> Double
probability i p pm vc a b =
  mul (pm `unsafeIndex` (Z :. i :. p)) (fromIntegral $ vc `unsafeIndex` (Z :. i :. p))
  where mul x y = (x**a) * (y**b)

-- | Takes index mapping to vertex data, set of blocked nodes,
-- | current route and remaining truck capacity then returns indexes of
-- | possible next customer nodes (that don't have greater demand than
-- | remaining truck capacity).
nextCustomers :: IndexVertexMap -> S.Set Int -> TruckCap -> [Int]
nextCustomers ivm blocked tc
  | tc <= 0 = []
  | otherwise = filter canVisit [0..end-1]
      where end = size $ extent ivm
            canVisit i | i `S.member` blocked = False
                       | otherwise = case ivm `unsafeIndex` (Z :. i) of
                            War {} -> False
                            Cus _ dem -> dem <= tc

-- | Takes index mapping to travel cost, pheromone map, a and b parameters,
-- | starting position, list of potential customers and a random generator.
-- | Returns next customer and new random generator.
selectNextCustomer :: RandomGen g =>
  IndexCostMap -> PheromoneMap -> Double -> Double ->
  Int -> [Int] -> g -> (Maybe Int, g)
selectNextCustomer icm pm a b start = probabilitySelect prob
  where prob c = probability start c pm icm a b

-- | Takes pheromone map, a parameter (since warehouse probability depends only
-- | on pheromone trail and not distance), list of warehouse nodes with their
-- | capacity and random generator.
-- | Returns selected warehouse and new random generator.
selectWarehouse :: RandomGen g =>
  PheromoneMap -> Double -> Double ->
  [(Int, Capacity)] -> g -> (Maybe (Int, Capacity), g)
selectWarehouse pm a b = probabilitySelect prob
  where prob (w, c) = (fromIntegral c ** b) * ((pm `unsafeIndex` (Z :. w :. w))**a)

-- | Function taking 2 node indexes and returning travel cost between the two
-- | and demand of the second one.
type CostDemand = Int -> Int -> (Cost, Demand)
-- | Function taking set of blocked nodes, remaining truck capacity and
-- | returns list of possible next customer nodes.
type NextCustomers = S.Set Int -> TruckCap -> [Int]
-- | Function taking start node index, list of next node indexes and a random
-- | generator then returns probability selected next node.
type SelectCustomer g = Int -> [Int] -> g -> (Maybe Int, g)

-- | Recursively construct a route for a truck.
-- | Takes functions for getting travel cost, demand, next possible customer
-- | nodes and probability selection, starting route, remaining truck capacity,
-- | set of visited nodes and random generator.
recurseRoute :: RandomGen g =>
  CostDemand -> NextCustomers -> SelectCustomer g ->
  Route -> TruckCap -> S.Set Int -> g ->
  (Route, S.Set Int, g)
recurseRoute cost gen sel r@(Route rns rc rd) tc visited rg =
  let nextCs = gen visited tc
      end = last $ routeNodes r
      next = if null nextCs then (Nothing, rg) else sel end nextCs rg
  in case next of
      (Nothing, rg') -> 
        let (travel, _) = cost (head $ routeNodes r) end
        in (Route rns (rc + travel) rd, visited, rg')
      (Just n, rg') ->
        let (travel, demand) = cost end n
            r' = Route (rns ++ [n]) (rc + travel) (rd + demand)
            tc' = tc - demand
        in recurseRoute cost gen sel r' tc' (S.insert n visited) rg'

-- | Constructs a route for a truck (recursively).
-- | Takes mappings from index to vertex data and travel costs;
-- | cost of using a truck, it's capacity and starting warehouse location;
-- | pheromone map with paremeters a and b;
-- | set of visited and/or blocked node indexes and random generator.
-- | Returns constructed route, new set of visited nodes and random generator.
constructRoute :: RandomGen g =>
  IndexVertexMap -> IndexCostMap ->
  TruckCost -> TruckCap -> Int ->
  PheromoneMap -> Double -> Double ->
  S.Set Int -> g ->
  (Route, S.Set Int, g)
constructRoute ivm icm truckCost truckCap wh pm a b visited rg =
  let cdf i j = (icm `unsafeIndex` (Z :. i :. j), elemDem $ ivm `unsafeIndex` (Z :. j))
      ncs = nextCustomers ivm
      sc = selectNextCustomer icm pm a b
      route = Route [wh] truckCost 0
      (route', visited', rg') = recurseRoute cdf ncs sc route truckCap visited rg
  in (bestRoutePairwisePermutation icm truckCost route', visited', rg')

-- | Returns the best permutated route using 2 opt exchange.
-- | Takes index mapping to travel cost, cost of sending a truck and a route.
bestRoutePairwisePermutation :: IndexCostMap -> TruckCost -> Route -> Route
bestRoutePairwisePermutation icm tc route@(Route rns rc _) =
  let wh = head rns
      perms = init . pairwisePermutation $ tail rns
      findBest best [] = best
      findBest best@(_, bc) (ns:nss) =
        let bc' = tc + calcTravelCost icm (wh:ns)
        in if bc' < bc then findBest (wh:ns, bc') nss else findBest best nss
      (bestNodes, cost) = findBest (rns, rc) perms
  in route { routeNodes = bestNodes, routeCost = cost }

-- | Generated a solution.
-- | Takes mappings from index to vertex data and travel costs;
-- | cost of using a truck, it's capacity and a list of warehouses with their
-- | remaining capacity;
-- | pheromone map with paremeters a and b;
-- | set of visited node indexes and a random generator and routes
-- | calclated so far.
-- | Returns a Solution and a random generator.
generateSolution :: RandomGen g =>
  IndexVertexMap -> IndexCostMap ->
  TruckCost -> TruckCap -> [(Int, Capacity)] ->
  PheromoneMap -> Double -> Double -> Double -> 
  S.Set Int -> g -> [Route] ->
  (Solution, S.Set Int, g)
generateSolution ivm icm truckCost truckCap ws pm a b bw visited rg rs =
  let wsn = length ws
      ableWs = filter (\(_,c) -> c > 0) ws
      end = size $ extent ivm  
      ((nextWh, whCap), rg') = case selectWarehouse pm a bw ableWs rg of
                                (Nothing, g) -> (head ableWs, g)
                                (Just w, g) -> (w, g)
      (r, visited', rg'') = constructRoute ivm icm truckCost (min truckCap whCap) nextWh pm a b visited rg'
      rd = routeDemand r
      (ws', rs') = if rd > 0 -- update warehouse capacity and delete empty routes
            then ((nextWh, whCap - rd) : delete (nextWh, whCap) ws, r:rs)
            else (ws, rs)
  in if S.fromList [wsn..end-1] == visited then
        let sc = calcSolutionCost ivm rs
        in  (Solution {routes = rs, solutionCost = sc}, visited, rg)
      else
        generateSolution ivm icm truckCost truckCap ws' pm a b bw visited' rg'' rs'

aco ::
  IndexVertexMap -> IndexCostMap ->
  TruckCost -> TruckCap -> Int ->
  PheromoneMap -> Double -> Double -> Double ->
  Double -> Double ->
  Float -> Int -> Int -> StdGen -> IO Solution
aco ivm icm trCost trCap nw pm a b bw evap deposit mutProb iter m rg =
  aco' (Solution [] 1000000) pm rg iter
  where ws = map (\i -> (i, elemCap $ ivm `unsafeIndex` (Z :. i))) [0..nw-1]
        map' = parMap rseq
        aco' !best !pm' !rg' !iter'
          | iter' == 0 = return best
          | solutionCost best <= solutionCost best' =
                        print (solutionCost best) >>
                        aco' best pm'' rg'' (iter' - 1)
          | otherwise = print (solutionCost best') >>
                        aco' best' pm'' rg'' (iter' - 1)
          where seeds = take m $ randoms rg'
                sols = map' (getSolution ivm icm trCost trCap ws pm a b bw) seeds
                muts = map' (getMutation ivm icm trCost nw mutProb) $ sols `zip` seeds
                best' = findBestSolution $ sols ++ muts
                pm'' = fromMaybe pm $ updatePheromoneMap best' pm' deposit evap
                rg'' = mkStdGen $ head seeds

getMutation ::
  IndexVertexMap -> IndexCostMap -> TruckCost ->
  Int -> Float -> (Solution, Int) -> Solution
getMutation ivm icm tc nw prob (sol, seed) =
  fst $ mutateSolution ivm icm tc nw prob (mkStdGen seed) sol

getSolution ::
  IndexVertexMap -> IndexCostMap ->
  TruckCost -> TruckCap -> [(Int, Capacity)] ->
  PheromoneMap -> Double -> Double -> Double -> 
  Int -> Solution
getSolution ivm icm truckCost truckCap ws pm a b bw s =
  (\(sol, _, _) -> sol) $
    generateSolution ivm icm truckCost truckCap ws pm a b bw S.empty (mkStdGen s) []

type ACOParameters = (Double, Double, Double, Int, Int, Float, Double, Double, Double)

solve :: Graph -> ACOParameters -> IO ()
solve g@(ws, _, truckCap, truckCost) parms = do
  
  let (a, b, bw, iter, m, mutProb, evap, deposit, initPher) = parms

  let vm = vertexMap g
  let vc = vertexCost vm
  let pm = initPheromoneMap vm initPher 
  let nw = length ws
  rg <- getStdGen

  {--
  let a = 0.1
  let b = -3.0 -- negative rewards short route, smaller negative bigger reward.
  let bw = -1.0
  let iter = 6000
  let m = 3000
  let mutProb = 0.15
  let evap = 0.1
  let deposit = 5.0E5
  --}
  sol <- aco vm vc truckCost truckCap nw pm a b bw evap deposit mutProb iter m rg
  putStr $ solutionToString (-nw) sol
