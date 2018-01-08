{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char
import System.IO

absoluteValue :: Int -> Int
absoluteValue n | n >= 0 = n
                | otherwise  = -n

isqrt = floor . sqrt . fromIntegral

square :: Int -> Int
square x = x * x

findSquares :: Int -> [Int] -> [Int]
findSquares _ [] = []
findSquares n (x:xs)
  | n > x = [x] ++ findSquares n xs
  | otherwise = [x] ++ findSquares n []

mids :: Int -> Int -> Int -> [Int]
mids min step0 step1 = let frst = min + step0
                       in [frst, frst + step1, frst + (2 * step1), frst + (3 * step1)]

findMids :: [Int] -> [Int]
findMids xs = let min = last $ init $ xs
                  max = last $ xs
                  root = isqrt max
                  step0 = div root 2
                  step1 = root - 1
              in mids min step0 step1

findsteps :: Int -> Int
findsteps 1 = 0
findsteps n = let squares = map square [1..n]
                  evenSquares = filter odd squares
                  listSquares = findSquares n evenSquares
                  lmids = findMids listSquares
                  fststp = minimum $ map absoluteValue $(map (\x -> (x - n)) lmids)
                  otherstps = (length listSquares) - 1
              in fststp + otherstps
------------------------------------------------------------------------------------------------------------------------------------
---- hint RULLDDRRRUUULLLLDDDDRRRRR == 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7...
nextDir :: Char -> Char
nextDir 'R' = 'U'
nextDir 'U' = 'L'
nextDir 'L' = 'D'
nextDir 'D' = 'R'

step :: Int -> [(Int, Int, Int, Int, Char)] -> [(Int, Int, Int, Int, Char)]
step 0 _ = [(0, 0, 1, 1, 'R')]
step n (x0: xs) = let (x, y, val, steps, dir) = x0
                      

-- nextNumber :: Int -> Int
-- nextNumber val = let result = -- somefunction
--                  in if result < val
--                     then
--                       result
--                     else
--                       -- somefunction + 1


-----------------------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
  input <-getLine
  let number = read input :: Int
  print $ findsteps number
  print $ 42
