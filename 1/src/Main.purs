module Main where

import Prelude

import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  text <- readFileAsUTF8 "day1.txt"
  nums <- pure ((map fromStringSafe <<< splitTextByNewline) text)
  log $ show (sum nums)

readFileAsUTF8 :: String -> Effect String
readFileAsUTF8 = readTextFile UTF8

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

getSafeNum :: Maybe Int -> Int
getSafeNum (Just x) = x
getSafeNum Nothing = 0

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString
