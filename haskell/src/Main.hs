import Solve
import System.Environment
import Data.List.Split

main = do
  args <- getArgs
  f <- readFile $ args!!0
  let spl = map ( map (splitOn "\t")) . map (splitOn "\r\n") . splitOn "\r\n\r\n" $ f
  putStrLn . show $ spl
