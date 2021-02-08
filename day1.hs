import Data.List
import Data.Maybe

parse :: String -> Int
parse [] = 0
parse ('+' : x) = read x
parse ('-' : x) = - read x

firstRepeatedWithSeen :: [Int] -> [Int] -> Int
firstRepeatedWithSeen seen (x : xs) =
  if x `elem` seen
    then x
    else firstRepeatedWithSeen (x : seen) xs

firstRepeated :: [Int] -> Int
firstRepeated = firstRepeatedWithSeen []

prefixSum :: Int -> [Int] -> [Int]
prefixSum a (x : xs) =
  x + a : prefixSum (x + a) xs

main = do
  contents <- getContents
  print (sum (map parse (lines contents)))
  let sums = prefixSum 0 (cycle (map parse (lines contents)))
   in print (firstRepeated sums)
