module Main where

import Prelude

import Data.Array (unsafeIndex)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  text <- readFileAsUTF8 "day3.txt"
  words <- pure $ splitTextByNewline text
  log "Part 1:"
  claims <- pure $ map getClaimFromString words
  log $ show claims

readFileAsUTF8 :: String -> Effect String
readFileAsUTF8 = readTextFile UTF8

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

type Claim = {
  id :: Int,
  left :: Int,
  right :: Int,
  width :: Int,
  height :: Int
}

recordPattern :: Regex
recordPattern = unsafeRegex "^#([0-9]*) @ ([0-9]*),([0-9]*): ([0-9]*)x([0-9]*)$" noFlags

matchRecord :: String -> Maybe (NonEmptyArray (Maybe String))
matchRecord = match recordPattern

getSafeString :: Maybe String -> String
getSafeString (Just x) = x
getSafeString Nothing = ""

getSafeNum :: Maybe Int -> Int
getSafeNum (Just x) = x
getSafeNum Nothing = 0

getSafeArray :: ∀ a. Maybe (NonEmptyArray a) -> Array a
getSafeArray (Just xs) = toArray xs
getSafeArray Nothing = []

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString

getClaimFromString :: String -> Claim
getClaimFromString = getClaimFromArray <<< map getSafeString <<< getSafeArray <<< matchRecord

getClaimFromArray :: Array String -> Claim
getClaimFromArray xs = {
  id: fromStringSafe (unsafePartial $ unsafeIndex xs 1),
  left: fromStringSafe (unsafePartial $ unsafeIndex xs 2),
  right: fromStringSafe (unsafePartial $ unsafeIndex xs 3),
  width: fromStringSafe (unsafePartial $ unsafeIndex xs 4),
  height: fromStringSafe (unsafePartial $ unsafeIndex xs 5)
}

