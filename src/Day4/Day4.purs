module Day4 where

import Prelude

import Data.Array (init, unsafeIndex)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Utilities (fromStringSafe, getSafeArray, getSafeArrayfromNonEmpty, getSafeString, splitTextByNewline)

part1 :: String -> String
part1 text = show $ map (getSleepRecordFromArray <<< map getSafeString <<< getSafeArrayfromNonEmpty <<< matchSleepRecord) $ getSafeArray $ init $ splitTextByNewline text

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
