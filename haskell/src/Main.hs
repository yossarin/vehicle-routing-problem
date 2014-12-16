import Data.List.Split (splitOn)
import Prelude
import System.Environment (getArgs)
import Solve

main = do
  args <- getArgs
  f <- readFile $ head args
  let spl = map (map (splitOn "\t") . splitOn "\r\n") . splitOn "\r\n\r\n" $ f
  print spl
