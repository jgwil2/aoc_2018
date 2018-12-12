module Day2
(
  part1,
  part2
)
where

import Prelude

import Data.Array (filter, groupBy, length, sort, zip)
import Data.Array.NonEmpty (NonEmptyArray, length) as NEA
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..), fst, snd)
import Utilities (splitTextByNewline)

part1 :: String -> String
part1 text = show $ (getNumberOfWordsContainingXLetters 2 words) * (getNumberOfWordsContainingXLetters 3 words)
  where
    words = splitTextByNewline text

part2 :: String -> String
part2 text = show $ map (joinWith "") $ map fst $ filter (\x -> arraysDifferByOne (fst x) (snd x)) $ cartesianProdSelf $ map wordToArray words
  where
    words = splitTextByNewline text

containsXOfAnyLetter :: Int -> String -> Boolean
containsXOfAnyLetter x str =
  let
    groupedLettersOfLengthX = filter (\a -> (NEA.length a) == x) $ groupLetters str
  in
    (length groupedLettersOfLengthX) > 0

groupLetters :: String -> Array (NEA.NonEmptyArray String)
groupLetters str = groupBy (==) <<< sort $ wordToArray str

wordToArray :: String -> Array String
wordToArray = split (Pattern "")

getNumberOfWordsContainingXLetters :: Int -> Array String -> Int
getNumberOfWordsContainingXLetters x words = length $ filter ((==) true) $ map (containsXOfAnyLetter x) words

arraysDifferByOne :: Array String -> Array String -> Boolean
arraysDifferByOne xs ys = length xs == length ys && 1 == length (getDifferingEls xs ys)

getDifferingEls :: Array String -> Array String -> Array String
getDifferingEls xs ys = map fst $ filter (\z -> (fst z) /= (snd z)) (zip xs ys)

-- NOTE: tried to give this function the most general type signature
-- possible, but it may be misnamed as a result. Does Cartesian product
-- apply to applicative functors other than list/set?
cartesianProd :: ∀ f a b. Bind f => Applicative f => f a -> f b -> f (Tuple a b)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure $ (Tuple x y)

cartesianProdSelf :: ∀ f a. Bind f => Applicative f => f a -> f (Tuple a a)
cartesianProdSelf xs = cartesianProd xs xs
