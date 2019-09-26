module Day4 where

import Prelude

import Data.Array (elemIndex, groupBy, init, sortBy, unsafeIndex, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.List (List(..), head)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Utilities (arrayToList, fromStringSafe, getSafeArray, getSafeArrayfromNonEmpty, getSafeNum, getSafeString, splitTextByNewline)

part1 :: String -> String
part1 text = show $ head splitRecords
  where
    words = splitTextByNewline text
    getSleepRecord = getSleepRecordFromArray <<< map getSafeString <<< getSafeArrayfromNonEmpty <<< matchSleepRecord
    records = map getSleepRecord words
    sortedRecords = sortBy compareSleepRecordDate records
    splitRecords = splitEvery 3 (arrayToList sortedRecords)

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


-- FIXME can this really not be made to work with arrays?
-- NOTE implementation cribbed from:
-- https://codereview.stackexchange.com/questions/48552/split-list-into-groups-of-n-in-haskell
splitEvery :: forall a. Int -> List a -> List (List a)
splitEvery _ Nil = Nil
splitEvery n xs = (Cons as) $ splitEvery n bs
  where (Tuple as bs) = splitAt n xs

-- NOTE implementation cribbed from:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#splitAt
splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt n ls
  | n <= 0 = (Tuple Nil ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: forall b. Int -> List b -> (Tuple (List b) (List b))
        splitAt' _  Nil     = (Tuple Nil Nil)
        splitAt' 1  (Cons x xs) = (Tuple ((Cons x) Nil) xs)
        splitAt' m  (Cons x xs) = (Tuple ((Cons x) xs') xs'')
          where
            (Tuple xs' xs'') = splitAt' (m - 1) xs
