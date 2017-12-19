{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char

-- usar reads :: [(Integer, String)]
-- usar words

checksum :: Int -> Int -> String -> Int
checksum max min [] = max - min
checksum max min (x0:x1:xs)
  | digitToInt x0 >= digitToInt x1 = checksum (digitToInt x0) (digitToInt x1) xs
  | digitToInt x0 < digitToInt x1 = checksum (digitToInt x1) (digitToInt x0) xs

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
  print $ input -- map (read :: String -> Int) (map words input)
