module Solve
( solve
, module Solve.Data
, vertexMap
, vertexCost
, initPheromoneMap
, evaporatePheromoneMap
, updatePheromoneMap
, probability
, nextCustomers
) where

import Data.Array.Repa hiding (map, (++), zipWith)
import qualified Data.Array.Repa.Operators.Mapping as M
import Data.Array.Repa.Repr.Vector (fromListVector, V)
import Data.List (foldl', groupBy, sort, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
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
type PheromoneMap   = Array U DIM2 Double

-- | Takes a reference to node index data mapping, cost of sending a truck,
-- | routes for each truck and calculates the total cost.
calcSolutionCost :: IndexVertexMap -> [Route] -> Cost
calcSolutionCost ivm rs =
  let traveling = sum $ map routeCost rs
      whs = usedWarehouses rs
      whCosts = sum $ map (\(i,_,_) -> elemCost $ ivm ! (Z :. i)) whs
  in traveling + whCosts

-- | Takes a node index data mapping and a solution. Returns True if all
-- | the routes' demand don't exceed the capacity of their warehouses.
canSupply :: IndexVertexMap -> Solution -> Bool
canSupply ivm (Solution rs _) = all canSupply' $ usedWarehouses rs
  where canSupply' (i, _, d) = d <= elemCap (ivm ! (Z :. i))

indexElemVer :: IndexVertexMap -> Int -> Vertex
indexElemVer ivm i = elemVer $ ivm ! (Z :. i)

-- | Takes node index data and index cost mappings, number of warehouses,
-- | mutation probability, random generator and a solution. Returns new
-- | solution which may have been mutated and if it can supply its routes.
mutateSolution :: RandomGen g =>
  IndexVertexMap -> IndexCostMap ->
  Int -> Float -> g -> Solution -> (Solution, g)
mutateSolution ivm icm whs prob rg s@(Solution rs _) =
  let (rs', rg') = mutateRoutes icm whs prob rg rs
      sc = calcSolutionCost ivm rs'
      s' = Solution rs' sc
  in if canSupply ivm s' then (s', rg') else (s, rg')

-- | Takes a node index cost mapping, number of warehouses, mutation
-- | mutation probability, random number generator and a list of routes.
-- | Returns new list of routes, of which some may have different warehouses.
mutateRoutes :: RandomGen g =>
  IndexCostMap ->
  Int -> Float -> g -> [Route] -> ([Route], g)
mutateRoutes icm whs prob rg = foldl' mutate ([], rg)
  where mutate (xs, g) x = (fromMaybe x x' : xs, g')
          where (x', g') = mutateRouteWarehouse icm whs prob g x

-- | Takes a node index cost mapping, number of warehouses, mutation
-- | probability, random number generator and a route. Returns new route if
-- | mutation happens, otherwise Nothing.
mutateRouteWarehouse :: RandomGen g =>
  IndexCostMap ->
  Int -> Float -> g -> Route -> (Maybe Route, g)
mutateRouteWarehouse icm whs prob rg r =
  let (rnd, rg') = randomR (0.0, 1.0) rg
      (newRoute, rg'') = randomRouteWarehouse icm whs rg' r
  in if prob > rnd then (Nothing, rg')
    else (Just newRoute, rg'')

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
      oldWhTravel = icm ! (Z :. oldWh :. start) + icm ! (Z :. oldWh :. end)
      newWhTravel = icm ! (Z :. w :. start) + icm ! (Z :. w :. end)
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
-- | ! Deprecated !
evaporatePheromoneMap :: Monad m => PheromoneMap -> Double -> m PheromoneMap
evaporatePheromoneMap ivm r = computeP $ M.map (\x -> (1-r)*x) ivm

-- | Takes Solution, PheromoneMap and parameters for adjusting the reinfocement
-- | and evaporation, then reinforces and evaporates the PheromoneMap according
-- | to the standard reinforcement and evaporation formula for ACO.
updatePheromoneMap :: Monad m =>
  Solution -> PheromoneMap -> Double -> Double -> m PheromoneMap
updatePheromoneMap s pm scal r = computeP $ traverse pm id update 
  where delta    = scal / fromIntegral (solutionCost s)
        edges    = concatMap (pairs . routeNodes) $ routes s
        pairs xs = zip xs $ tail $ cycle xs -- returning to warehouse
        update get (Z :. i :. j) = 
          if (i,j) `elem` edges then 
            get (Z :. i :. j)*(1-r) + delta
          else
            get (Z :. i :. j)*(1-r)

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

-- | Takes initial position, potential, PheromoneMap, IndexCostMap,
-- | parameter a and b and calculates the probability of choosing
-- | the potential position as a next hop via standard formula for ACO.
-- | The probability isn't scaled to [0, 1] interval.
probability :: Int -> Int 
  -> PheromoneMap -> IndexCostMap
  -> Double -> Double -> Double
probability i p pm vc a b =
  mul (pm ! (Z :. i :. p)) (fromIntegral $ vc ! (Z :. i :. p))
  where (Z :. end :. _) = extent vc
        mul x y = x**a + y**b

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
                       | otherwise = case ivm ! (Z :. i) of
                            War {} -> False
                            Cus _ dem -> dem <= tc
solve :: Graph -> IO ()
solve _ = putStrLn "Bazzzzzinga!"
