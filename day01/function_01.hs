import System.IO
import Data.Char

readString :: String -> [Int]
readString [] = []
readString (x:xs) 
  | x == '\n' = readString xs
  | otherwise = [digitToInt x] ++ readString xs

sumRepeated :: [Int] -> Int
sumRepeated (x:z:zs)
  | x == z = x + sumRepeated (z:zs)
  | otherwise = sumRepeated (z:zs)
sumRepeated _ = 0

main = do
  content <- readFile "/home/chagas/Documents/haskell-try/advetofcode/day01/input.txt"
  let l = readString content
      final = sumRepeated (l ++ (take 1 l))

  print final
