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

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ head args

  solve $ parse f

