import System.IO
import Data.Char

sumRepeated :: String -> Integer
sumRepeated (x:xs) = let a = digitToInt x
                     print a


main = do
  content <- readFile "/home/chagas/Documents/haskell-try/advetofcode/day01/input.txt"
  let c = head content
      x = digitToInt c
  print x 
