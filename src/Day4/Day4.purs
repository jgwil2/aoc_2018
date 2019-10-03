module Day4 where

import Prelude

import Data.Array (filter, head, sortBy, span, unsafeIndex)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Utilities (fromStringSafe, getSafeArrayfromNonEmpty, getSafeString, splitTextByNewline)

part1 :: String -> String
part1 text = show $ map getGuardIDFromBeginShiftRec $ filter isBeginShiftRec sortedRecords
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

-- getGuardAndSleepTimes :: Array SleepRecord -> Array GuardAndSleepTimes
-- getGuardAndSleepTimes [] = []
-- TODO recursive function which passes guard id as parameter and uses
-- span (?) to grab all successive records until another guard id is
-- found, then calls itself with the new guard id and (init spanned.rest)

beginShiftPattern :: Regex
beginShiftPattern = unsafeRegex "Guard #([0-9]+) begins shift" noFlags

isBeginShiftRec :: SleepRecord -> Boolean
isBeginShiftRec rec = test beginShiftPattern rec.message

getGuardIDFromBeginShiftRec :: SleepRecord -> Int
getGuardIDFromBeginShiftRec rec =
  let matches = match beginShiftPattern rec.message
  in
    fromStringSafe (unsafePartial $ unsafeIndex (map getSafeString $ getSafeArrayfromNonEmpty matches) 1)
