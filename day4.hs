import Chronos
  ( Datetime,
    OffsetFormat (OffsetFormatColonOff),
    datetimeFromYmdhms,
    datetimeTime,
    datetimeToTime,
    decodeOffset,
    getTimespan,
    timeIntervalBuilder,
    timeIntervalToTimespan,
    timeOfDayMinute,
    timeParts,
    timePartsMinute,
  )
import Data.Char
import Data.List
import Data.Tuple

data Event = Begin Datetime Int | Sleep Datetime | Wake Datetime deriving (Show)

type ShiftEvents = [Event]

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)

toTime :: String -> Datetime
toTime line =
  let (year : month : day : hour : minute : _) = map read (words (map (\c -> if isNumber c then c else ' ') line))
   in datetimeFromYmdhms year month day hour minute 0

getMinute :: Datetime -> Int
getMinute =
  timeOfDayMinute . datetimeTime

deltaTime :: Datetime -> Datetime -> Int
deltaTime t1 t2 =
  fromIntegral
    ( ( getTimespan
          . timeIntervalToTimespan
          $ timeIntervalBuilder (datetimeToTime t1) (datetimeToTime t2)
      )
        `div` 1000000000
        `div` 60
    )

extractBeginId :: String -> Int
extractBeginId line =
  read (tail (words line !! 3))

toEvent :: String -> Event
toEvent line
  | "wakes" `elem` words line =
    Wake (toTime line)
  | "asleep" `elem` words line =
    Sleep (toTime line)
  | otherwise =
    Begin (toTime line) (extractBeginId line)

join :: String -> String -> String
join a b = a ++ "\n" ++ b

partitionByShiftChange :: [Event] -> ([Event], [Event])
partitionByShiftChange [] = ([], [])
partitionByShiftChange (Begin time id : xs) = ([], Begin time id : xs)
partitionByShiftChange (x : xs) =
  let (part, remaining) = partitionByShiftChange xs
   in (x : part, remaining)

splitByShiftChanges :: [Event] -> [[Event]]
-- extract until empty or next shift beginning
splitByShiftChanges [] = []
splitByShiftChanges (Begin time id : xs) =
  let (part, remaining) = partitionByShiftChange xs
   in (Begin time id : part) : splitByShiftChanges remaining
splitByShiftChanges _ = error "There should not be a wake/sleep without a shift beginning"

shiftId :: ShiftEvents -> Int
shiftId [] = error "There should not be an empty shift"
shiftId (Begin time id : _) = id
shiftId _ = error "shifts must begin with a beginning"

shiftSleepingDuration :: ShiftEvents -> Int
shiftSleepingDuration [] = 0
shiftSleepingDuration (Begin _ _ : rest) = shiftSleepingDuration rest
shiftSleepingDuration (Sleep time1 : Wake time2 : rest) =
  deltaTime time1 time2 + shiftSleepingDuration rest
shiftSleepingDuration _ = error "Malformed inputs"

merge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
merge [] (id, duration) = [(id, duration)]
merge ((last_id, last_duration) : rest) (id, duration)
  | last_id == id = (id, duration + last_duration) : rest
  | otherwise = (id, duration) : (last_id, last_duration) : rest

shiftMinutes :: ShiftEvents -> [Int]
shiftMinutes [] = []
shiftMinutes (Begin _ _ : rest) = shiftMinutes rest
shiftMinutes (Sleep time1 : Wake time2 : rest) =
  let dur = deltaTime time1 time2
      start_minute = getMinute time1
   in [start_minute .. start_minute + dur - 1] ++ shiftMinutes rest
shiftMinutes _ = error "Malformed inputs"

findFreqSleepingMinute :: [ShiftEvents] -> [(Int, Int)]
findFreqSleepingMinute = encode . sort . concatMap shiftMinutes

main = do
  contents <- getContents
  let events = map toEvent . sort . lines $ contents
      shifts = splitByShiftChanges events
      all_guards = nub . map shiftId $ shifts
      (hours, guard_id) = maximum . map swap . foldl merge [] . sort $ [(shiftId s, shiftSleepingDuration s) | s <- shifts]
      guard_shifts = [s | s <- shifts, shiftId s == guard_id]
      (freq, freq_min) = maximum . findFreqSleepingMinute $ guard_shifts
      ((freq2, freq_min2), id2) = maximum [(maximum ((0, -1) : findFreqSleepingMinute [s | s <- shifts, shiftId s == gid]), gid) | gid <- all_guards]
   in do
        -- putStrLn . foldl1 join . map show $ events
        print (guard_id * freq_min)
        print all_guards
        print (freq_min2 * id2)

-- print $ sort $ lines contents