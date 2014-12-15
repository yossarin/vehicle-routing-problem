import Solve
import System.Environment

main = do
  args <- getArgs
  f <- readFile $ args!!0
  putStr f
