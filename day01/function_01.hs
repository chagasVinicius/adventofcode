import System.IO
import Data.Char

-- sumRepeated :: String -> [Int]
-- sumRepeated [] = []
-- sumRepeated (x:xs) = [digitToInt x]

readString :: String -> [Int]
readString [] = []
readString (x:xs) = [digitToInt x] ++ readString xs

sumRepeated :: String -> String
sumRepeated [] = []
sumRepeated [x] = []
sumRepeated (x:z:zs)
  | x == z = [x] ++ sumRepeated (zs)
  | otherwise = sumRepeated (x:zs)

main = do
  content <- readFile "/home/chagas/Documents/haskell-try/advetofcode/day01/input.txt"
  let s = sumRepeated content
      l = readString s
      final = sum l

  print final
