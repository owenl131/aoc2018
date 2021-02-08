import Data.List

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

hasX :: Int -> String -> Bool
hasX count str =
  any (\(x, _) -> x == count) (encode (sort str))

countUnmatches :: String -> String -> Int
countUnmatches [] [] = 0
countUnmatches (x : xs) (y : ys) =
  countUnmatches xs ys + if x /= y then 1 else 0

main = do
  contents <- getContents
  -- Part 1
  print (length (filter (hasX 2) (lines contents)) * length (filter (hasX 3) (lines contents)))
  -- Part 2
  print
    [ (x, y)
      | x <- lines contents,
        y <- lines contents,
        countUnmatches x y == 1,
        x < y
    ]