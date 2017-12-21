{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char
import System.IO

checksum :: Int -> Int -> [Int] -> Int
checksum max min [] = max - min
checksum max min (x0:xs)
  | x0 >= max = checksum x0 min xs
  | x0 < min = checksum max x0 xs
  | otherwise = checksum max min xs

checksum' :: Int -> Int -> [Int] -> Int
checksum' _ res [] = res
checksum' div_ res (x0:xs)
  | mod div_ x0 == 0 = checksum' div_ (div div_ x0) xs
  | mod x0 div_ == 0 = checksum' div_ (div x0 div_) xs
  | otherwise = checksum' div_ res xs

totalSum :: [Int] -> [Int]
totalSum [] = []
totalSum (x:xs) = [checksum' x 0 xs] ++ totalSum xs

slistToInt :: [String] -> [Int]
slistToInt (xs) = map (read :: String -> Int) xs

getLines :: IO [String]
getLines = isEOF >>= \eof ->
  if eof
  then pure []
  else do
    line <- getLine
    (line :) <$> getLines
    
main :: IO()
main = do
  input <- getLines
  let sinput = map words input
      iinput = map slistToInt sinput
  print $ sum (map (\(x0:xs) -> checksum x0 x0 xs) iinput)
  print $ sum (map sum (map totalSum iinput))
