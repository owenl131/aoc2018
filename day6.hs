import Data.List
import Data.Maybe
import Data.Tuple

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

processLine :: String -> (Int, Int)
processLine line =
  let (a : b : _) = words line
   in (read . init $ a, read b)

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coveredBy :: [(Int, Int)] -> (Int, Int) -> Maybe Int
coveredBy coords point =
  let ((dist1, index1) : (dist2, index2) : _) =
        sort . flip zip [0 ..] . map (manhattanDist point) $ coords
   in if dist1 == dist2 then Nothing else Just index1

totalDistance :: [(Int, Int)] -> (Int, Int) -> Int
totalDistance coords point =
  sum . map (manhattanDist point) $ coords

main = do
  contents <- getContents
  let lowerbound = -200
      upperbound = - lowerbound + 400
      lowerbound2 = -1500
      upperbound2 = - lowerbound + 400
      coords = map processLine . lines $ contents
      infinites :: [Int]
      infinites =
        nub
          . mapMaybe (coveredBy coords)
          $ [(lowerbound, k) | k <- [lowerbound .. upperbound]]
            ++ [(upperbound, k) | k <- [lowerbound .. upperbound]]
            ++ [(k, lowerbound) | k <- [lowerbound .. upperbound]]
            ++ [(k, upperbound) | k <- [lowerbound .. upperbound]]
      -- counts :: [(Int, Int)]
      counts =
        maximum
          . filter (flip notElem infinites . snd)
          . encode
          . sort
          . mapMaybe (coveredBy coords)
          $ [ (i, j)
              | i <- [lowerbound + 1 .. upperbound - 1],
                j <- [lowerbound + 1 .. upperbound - 1]
            ]
      matchingCriteria =
        length
          . filter (10000 >)
          . map (totalDistance coords)
          $ [ (i, j)
              | i <- [lowerbound2 .. upperbound2],
                j <- [lowerbound2 .. upperbound2]
            ]
   in do
        -- print coords
        print counts
        print matchingCriteria