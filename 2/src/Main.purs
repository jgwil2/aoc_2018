module Main where

import Prelude

import Data.Array (length, filter, groupBy, sort)
import Data.Array.NonEmpty (NonEmptyArray, length) as NEA
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readFileAsUTF8 "day2.txt"
  words <- pure (splitTextByNewline text)
  log $ show $ (getNumberOfWordsContainingXLetters 2 words) * (getNumberOfWordsContainingXLetters 3 words)

readFileAsUTF8 :: String -> Effect String
readFileAsUTF8 = readTextFile UTF8

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

containsXOfAnyLetter :: Int -> String -> Boolean
containsXOfAnyLetter x str =
  let
    groupedLettersOfLengthX = filter (\a -> (NEA.length a) == x) $ groupLetters str
  in
    (length groupedLettersOfLengthX) > 0

groupLetters :: String -> Array (NEA.NonEmptyArray String)
groupLetters str = groupBy (==) <<< sort $ split (Pattern "") str

contains2OfAnyLetter :: String -> Boolean
contains2OfAnyLetter = containsXOfAnyLetter 2

contains3OfAnyLetter :: String -> Boolean
contains3OfAnyLetter = containsXOfAnyLetter 3

getNumberOfWordsContainingXLetters :: Int -> Array String -> Int
getNumberOfWordsContainingXLetters x words = length $ filter ((==) true) $ map (containsXOfAnyLetter x) words
