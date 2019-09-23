module Day1
(
  part1,
  part2
)
where

import Prelude

import Data.Array (cons, difference, head, last, reverse, tail, init)
import Data.Foldable (find, foldl, sum)
import Data.Maybe (Maybe, isJust)
import Utilities (fromStringSafe, getSafeArray, getSafeNum, splitTextByNewline)

part1 :: String -> String
part1 text = show $ sum $ getNums text

part2 :: String -> String
part2 text = show $ getFirstRepeating (getSafeArray (init (getRunningTotal nums startingTotals)))
  where
    nums = getNums text
    startingTotals = [0]

getNums :: String -> Array Int
getNums = map fromStringSafe <<< splitTextByNewline

getRunningTotal :: Array Int -> Array Int -> Array Int
getRunningTotal [] totals = reverse totals
getRunningTotal xs totals =
  getRunningTotal (getSafeArray (tail xs)) (cons ((getSafeNum (head xs)) + (getSafeNum (last (reverse totals)))) totals)

-- TODO write a recursive function that checks if `getFirstRepeating` has a
-- value and if it doesn't, cycle through the input again

getFirstRepeating :: ∀ a. Eq a => Array a -> Maybe a
getFirstRepeating = head <<< nonUniq

nonUniq :: ∀ a. Eq a => Array a -> Array a
nonUniq xs = difference xs (uniq xs)

uniq :: ∀ a. Eq a => Array a -> Array a
uniq = reverse <<< foldl (\acc x -> if contains x acc then acc else cons x acc) []

contains :: ∀ a. Eq a => a -> Array a -> Boolean
contains x xs = isJust found
  where found = find (\y -> y == x) xs
