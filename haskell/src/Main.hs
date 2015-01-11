import Data.List.Split (splitOn)
import Prelude
import System.Environment (getArgs)
import Solve

listify :: String -> [[[String]]]
listify = filter (/= [[""]]) . map (map (splitOn "\t") . splitOn "\n") . splitOn "\n\n"

readNumbers :: [[[String]]] -> [[[Int]]]
readNumbers =  map (map (map read))

parse :: String -> Graph
parse f = do
  let groupedFIle = readNumbers $ listify f
  let warehouses = zip3 (map (\[x,y] -> (x,y)) $ groupedFIle!!1) (concat $ groupedFIle!!4) (concat $ groupedFIle!!6)
  let customers = zip (map (\[x,y] -> (x,y)) $ groupedFIle!!2) (concat $ groupedFIle!!5)
  let truckCap  = head . head $ groupedFIle!!3
  let truckCos  = head . head $ groupedFIle!!7
  (warehouses, customers, truckCap, truckCos)

getACOParameters :: FilePath -> IO ACOParameters
getACOParameters fp  = do
  f <- readFile fp
  let fourthLine = lines f !! 3
  let p          = words fourthLine
  
  let a        = (read $ head p) :: Double
  let b        = (read $ p!!1) :: Double
  let bw       = (read $ p!!2) :: Double
  let iter     = (read $ p!!3) :: Int
  let m        = (read $ p!!4) :: Int
  let mutProb  = (read $ p!!5) :: Float
  let evap     = (read $ p!!6) :: Double
  let deposit  = (read $ p!!7) :: Double
  let initPher = (read $ p!!8) :: Double
  
  return (a, b, bw, iter, m, mutProb, evap, deposit, initPher)

defaultAcoParameters :: IO ACOParameters
defaultAcoParameters = return (0.05, -0.05, -1.0, 2000, 18, 0.1, 0.8, 1.0, 2.0)

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ head args
  
  let p = if length args > 1 then getACOParameters $ args!!1             else defaultAcoParameters
    
  parms <- p

  solve (parse f) parms

