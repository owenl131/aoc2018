import Data.Char

isOpposite :: Char -> Char -> Bool
isOpposite a b = toUpper a == toUpper b && a /= b

collapseChainHelper :: String -> String -> String
-- processed, unprocessed, result
collapseChainHelper [] [] = []
collapseChainHelper processed [] = processed
collapseChainHelper [] (x : xs) = collapseChainHelper [x] xs
collapseChainHelper (p : ps) (x : xs)
  | isOpposite p x = collapseChainHelper ps xs
  | otherwise = collapseChainHelper (x : p : ps) xs

collapseChain :: String -> String
collapseChain = collapseChainHelper ""

main = do
  contents <- getContents
  let input = filter (not . isSpace) contents
   in do
        print . length . collapseChain . filter (not . isSpace) $ contents
        print . minimum $
          [ length
              . collapseChain
              . filter (not . (==) c . toLower)
              $ input
            | c <- ['a' .. 'z']
          ]