module Day1
(
  part1,
  part2
)
where

import Prelude

import Data.Array (cons, difference, head, init, last, reverse, snoc, tail)
import Data.Foldable (find, foldl, sum)
import Data.Maybe (Maybe(..), isJust)
import Utilities (fromStringSafe, getSafeArray, getSafeNum, splitTextByNewline)

part1 :: String -> String
part1 text = show $ sum $ getNums text

part2 :: String -> String
part2 text = show $ f firstRepeating startingTotals nums
  where
    nums = getNums text
    startingTotals = [0]
    firstRepeating = getFirstRepeating (getRunningTotal nums startingTotals)

getNums :: String -> Array Int
getNums = map fromStringSafe <<< splitTextByNewline

getRunningTotal :: Array Int -> Array Int -> Array Int
getRunningTotal [x] totals = totals
getRunningTotal xs totals =
  getRunningTotal (tail' xs) (snoc totals (last' totals + head' xs))
  where
    tail' = getSafeArray <<< tail
    last' = getSafeNum <<< last
    head' = getSafeNum <<< head

f :: Maybe Int -> Array Int -> Array Int -> Int
f (Just x) runningTotals nums = x
f Nothing runningTotals nums = f (getFirstRepeating newTotals) newTotals nums
  where
    newTotals = getRunningTotal nums runningTotals

getFirstRepeating :: ∀ a. Eq a => Array a -> Maybe a
getFirstRepeating = head <<< nonUniq

nonUniq :: ∀ a. Eq a => Array a -> Array a
nonUniq xs = difference xs (uniq xs)

uniq :: ∀ a. Eq a => Array a -> Array a
uniq = reverse <<< foldl (\acc x -> if contains x acc then acc else cons x acc) []

contains :: ∀ a. Eq a => a -> Array a -> Boolean
contains x xs = isJust found
  where found = find (\y -> y == x) xs
