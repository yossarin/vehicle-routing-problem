import Data.List.Split (splitOn)
import Prelude
import System.Environment (getArgs)
import Solve

listify :: String -> [[[String]]]
listify = filter (/= [[""]]) . map ( map (splitOn "\t")) . map (splitOn "\r\n") . splitOn "\r\n\r\n"

readNumbers :: [[[String]]] -> [[[Int]]]
readNumbers =  map (map (map read))

main :: IO ()
main = do
  args <- getArgs
  f <- readFile $ args!!0

  let groupedFIle = readNumbers $ listify f
  let warehouses = zip3 (map (\[x,y] -> (x,y)) $ groupedFIle!!1) (concat $ groupedFIle!!4) (concat $ groupedFIle!!6)
  let customers = zip (map (\[x,y] -> (x,y)) $ groupedFIle!!2) (concat $ groupedFIle!!5)
  let truckCap  = head . head $ groupedFIle!!3
  let truckCos  = head . head $ groupedFIle!!7
  
  putStrLn $ "Warehouses: " ++ ( show  warehouses)
  putStrLn $ "\nCustomers: " ++ (show  customers)
  putStrLn $ "\ntruckCap & Cos: " ++ (show truckCap) ++" "++ (show truckCos) 
  solve (warehouses, customers, truckCap, truckCos)

