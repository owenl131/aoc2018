import Data.Char
import Data.List
import Data.Maybe

data Square = Square Int Int Int Int Int
  deriving (Show)

squareId :: Square -> Int
squareId (Square x _ _ _ _) = x

toSquares :: String -> Square
toSquares line =
  let [id, a, b, c, d] = map read (words (map (\c -> if isNumber c then c else ' ') line))
   in Square id a b (a + c - 1) (b + d - 1)

willIntersect :: Square -> Square -> Bool
willIntersect (Square _ x1 y1 x2 y2) (Square _ a1 b1 a2 b2) =
  not (a1 > x2 || b1 > y2 || x1 > a2 || y1 > b2)

squareIntersect :: Square -> Square -> Maybe Square
squareIntersect (Square id1 x1 y1 x2 y2) (Square id2 a1 b1 a2 b2) =
  if willIntersect (Square id1 x1 y1 x2 y2) (Square id2 a1 b1 a2 b2)
    then Just (Square (id1 * 10000 + id2) (max x1 a1) (max y1 b1) (min x2 a2) (min y2 b2))
    else Nothing

squareToPoints :: Square -> [(Int, Int)]
squareToPoints (Square _ x1 y1 x2 y2) =
  [ (x, y)
    | x <- [x1 .. x2],
      y <- [y1 .. y2]
  ]

main = do
  contents <- getContents
  let squares = map toSquares (lines contents)
   in do
        -- Part 2
        print (filter (\s -> not (any (willIntersect s) [x | x <- squares, squareId x /= squareId s])) squares)
        -- Part 1
        print
          ( length
              ( foldl1
                  union
                  ( map
                      squareToPoints
                      (catMaybes [squareIntersect a b | a <- squares, b <- squares, squareId a < squareId b])
                  )
              )
          )