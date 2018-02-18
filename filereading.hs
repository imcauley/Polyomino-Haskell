import System.IO
import System.Environment
import nomimo

main = do
  contents <- readFile "6.txt"
  putStrLn contents
