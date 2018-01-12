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
-- HINT Look Data.Map Haskell we can create a Map with the coords (x,y) as keys and the values of each cell
timesDir :: (Int, Char) -> (Int, Char)
timesDir (count, dir)
  | dir == 'R' = (count, 'U')
  | dir == 'U' = (count + 1, 'L')
  | dir == 'L' = (count, 'D')
  | dir == 'D' = (count + 1, 'R')

walking :: Char -> (Int, Int) -> (Int, Int)
walking 'R' (x,y) = (x + 1, y)
walking 'L' (x,y) = (x - 1, y)
walking 'U' (x,y) = (x, y + 1)
wlaking 'D' (x,y) = (x, y -1)

timesSteps :: [(Int, Char)] -> (Int, Char)
timesSteps (x0:xs) = timesDir x0


-----------------------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
  input <-getLine
  let number = read input :: Int
  print $ findsteps number
  print $ 42
