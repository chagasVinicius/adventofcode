{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char
import System.IO

-- usar reads :: [(Integer, String)]
-- usar words

checksum :: Int -> Int -> [Integer] -> Int
checksum max min [] = max - min
checksum max min (x0:x1:xs)
  | 

slistToInt :: [String] -> [Integer]
slistToInt (xs) = map (read :: String -> Integer) xs

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
  print $ iinput
