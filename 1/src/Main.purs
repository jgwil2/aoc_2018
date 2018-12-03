module Main where

import Prelude

import Data.Array (cons, difference, head, last, reverse, tail)
import Data.Foldable (class Foldable, find, foldl, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readFileAsUTF8 "day1.txt"
  nums <- pure ((map fromStringSafe <<< splitTextByNewline) text)
  log $ show (getFirstRepeating (getRunningTotal nums [0]))

readFileAsUTF8 :: String -> Effect String
readFileAsUTF8 = readTextFile UTF8

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

getSafeNum :: Maybe Int -> Int
getSafeNum (Just x) = x
getSafeNum Nothing = 0

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString

getTotal :: ∀ a f. Foldable f ⇒ Semiring a ⇒ f a → a
getTotal = sum

getRunningTotal :: Array Int -> Array Int -> Array Int
getRunningTotal [] totals = reverse totals
getRunningTotal xs totals =
  getRunningTotal (getSafeArray (tail xs)) (cons ((getSafeNum (head xs)) + (getSafeNum (last (reverse totals)))) totals)

getSafeArray :: ∀ a. Maybe (Array a) -> Array a
getSafeArray (Just xs) = xs
getSafeArray Nothing = []

getFirstRepeating :: ∀ a. Eq a => Array a -> Maybe a
getFirstRepeating = head <<< nonUniq

nonUniq :: ∀ a. Eq a => Array a -> Array a
nonUniq xs = difference xs (uniq xs)

uniq :: ∀ a. Eq a => Array a -> Array a
uniq = reverse <<< foldl (\acc x -> if contains x acc then acc else cons x acc) []

contains :: ∀ a. Eq a => a -> Array a -> Boolean
contains x xs = isJust found
  where found = find (\y -> y == x) xs
