import System.IO

main = do
  content <- readFile "/home/chagas/Documents/haskell-try/advetofcode/day01/input.txt"
  let  c = head content
  print c
