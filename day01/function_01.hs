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

sumRepeated' :: Int -> [Int] -> [Int] -> Int
sumRepeated' acc [] [] = acc
sumRepeated' acc (x:xs) (y:ys)
  | x == y = sumRepeated' (acc + (x + y)) xs ys
  | otherwise = sumRepeated' acc xs ys

main = do
  content <- readFile "/home/chagas/Documents/haskell-try/advetofcode/day01/input.txt"
  let l = readString content
      final1 = sumRepeated (l ++ (take 1 l))
      split = div (length l) 2
      (l0, l1) = splitAt split l
      final2 = sumRepeated' 0 l0 l1

  print (final1, final2)
