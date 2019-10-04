module Day4 where

import Prelude

import Data.Array (filter, head, sortBy, unsafeIndex)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.List (List(..), concat, span)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Utilities (arrayToList, fromStringSafe, getSafeArrayfromNonEmpty, getSafeList, getSafeString, splitTextByNewline)

part1 :: String -> String
part1 text = show $ buildMap empty $ arrayToList sortedRecords
  where
    words = splitTextByNewline text
    getSleepRecord = getSleepRecordFromArray <<< map getSafeString <<< getSafeArrayfromNonEmpty <<< matchSleepRecord
    records = map getSleepRecord words
    sortedRecords = sortBy compareSleepRecordDate records

part2 :: String -> String
part2 text = show text

type SleepRecord = {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  message :: String
}

sleepRecordPattern :: Regex
sleepRecordPattern = unsafeRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] ([a-zA-Z0-9\\s#]*)" noFlags

matchSleepRecord :: String -> Maybe (NonEmptyArray (Maybe String))
matchSleepRecord = match sleepRecordPattern

getSleepRecordFromArray :: Array String -> SleepRecord
getSleepRecordFromArray xs = {
  year: fromStringSafe (unsafePartial $ unsafeIndex xs 1),
  month: fromStringSafe (unsafePartial $ unsafeIndex xs 2),
  day: fromStringSafe (unsafePartial $ unsafeIndex xs 3),
  hour: fromStringSafe (unsafePartial $ unsafeIndex xs 4),
  minute: fromStringSafe (unsafePartial $ unsafeIndex xs 5),
  message: (unsafePartial $ unsafeIndex xs 6)
}

-- HACK: syntactic contortions abound: see
-- https://github.com/purescript/purescript/issues/888
compareSleepRecordDate :: SleepRecord -> SleepRecord -> Ordering
compareSleepRecordDate r1 r2 =
  let monthsComp = compare r1.month r2.month
      daysComp = compare r1.day r2.day
      hoursComp = compare r1.hour r2.hour
      minutesComp = compare r1.minute r2.minute
  in case unit of
    _ | monthsComp /= EQ -> monthsComp
    _ | daysComp /= EQ -> daysComp
    _ | hoursComp /= EQ -> hoursComp
    _ | otherwise -> minutesComp

type GuardAndSleepTimes = {
  id :: Int,
  wentToSleep :: Int,
  wokeUp :: Int
}

buildMap :: Map Int (List SleepRecord) -> List SleepRecord -> Map Int (List SleepRecord)
buildMap recordsMap Nil = recordsMap
buildMap recordsMap (Cons rec recs) =
  let vals = span (not isBeginShiftRec) recs
      id = getGuardIDFromBeginShiftRec rec
      newMap = insertOrConcat id vals.init recordsMap
  in
    buildMap newMap vals.rest

insertOrConcat :: forall a. Int -> List a -> Map Int (List a) -> Map Int (List a)
insertOrConcat id vals recordsMap = if member id recordsMap
  then insert id concatenatedList recordsMap
  else insert id vals recordsMap
  where
    currentVals = lookupSafe id recordsMap
    concatenatedList = concat (Cons currentVals (Cons vals Nil))

lookupSafe :: forall a b. Ord a => a -> Map a (List b) -> List b
lookupSafe k m = getSafeList $ lookup k m

beginShiftPattern :: Regex
beginShiftPattern = unsafeRegex "Guard #([0-9]+) begins shift" noFlags

isBeginShiftRec :: SleepRecord -> Boolean
isBeginShiftRec rec = test beginShiftPattern rec.message

getGuardIDFromBeginShiftRec :: SleepRecord -> Int
getGuardIDFromBeginShiftRec rec =
  let matches = match beginShiftPattern rec.message
  in
    fromStringSafe (unsafePartial $ unsafeIndex (map getSafeString $ getSafeArrayfromNonEmpty matches) 1)
