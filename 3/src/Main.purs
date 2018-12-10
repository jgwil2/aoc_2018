module Main where

import Prelude

import Data.Array (concatMap, filter, group, length, sort, unsafeIndex, (..))
import Data.Array.NonEmpty (length, toArray) as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
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
  totalPoints <- pure $ concatMap getPointsFromString words
  groupedPoints <- pure $ group $ sort totalPoints
  log $ show $ length $ filter (\x -> 1 /= NEA.length x) groupedPoints

readFileAsUTF8 :: String -> Effect String
readFileAsUTF8 = readTextFile UTF8

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

type ID = Int

type Claim = {
  id :: ID,
  left :: Int,
  top :: Int,
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

getSafeArrayfromNonEmpty :: ∀ a. Maybe (NonEmptyArray a) -> Array a
getSafeArrayfromNonEmpty (Just xs) = NEA.toArray xs
getSafeArrayfromNonEmpty Nothing = []

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString

getPointsFromString :: String -> Array Point
getPointsFromString =
  getPointsFromClaim <<< getClaimFromArray <<< map getSafeString <<<
  getSafeArrayfromNonEmpty <<< matchRecord

getClaimFromArray :: Array String -> Claim
getClaimFromArray xs = {
  id: fromStringSafe (unsafePartial $ unsafeIndex xs 1),
  left: fromStringSafe (unsafePartial $ unsafeIndex xs 2),
  top: fromStringSafe (unsafePartial $ unsafeIndex xs 3),
  width: fromStringSafe (unsafePartial $ unsafeIndex xs 4),
  height: fromStringSafe (unsafePartial $ unsafeIndex xs 5)
}

data Point = Point {
  x :: Int,
  y :: Int
}

instance showPoint :: Show Point where
  show (Point p) = "(Point " <> show p.x <> ", " <> show p.y <> ")"

instance eqPoint :: Eq Point where
  eq (Point p) (Point q) = p.x == q.x && p.y == q.y

instance ordPoint :: Ord Point where
  compare (Point p) (Point q)
    | p.x > q.x = GT
    | p.x < q.x = LT
    | p.x == q.x && p.y > q.y = GT
    | p.x == q.x && p.y < q.y = LT
  compare _ _ = EQ

getPointsFromClaim :: Claim -> Array Point
getPointsFromClaim claim =
  let xs = claim.left..(claim.left + claim.width - 1)
      ys = claim.top..(claim.top + claim.height - 1)
      tupleToPoint :: (Tuple Int Int) -> Point
      tupleToPoint (Tuple x y) = Point { x: x, y: y }
  in
      map tupleToPoint $ cartesianProd xs ys

cartesianProd :: ∀ f a b. Bind f => Applicative f => f a -> f b -> f (Tuple a b)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure $ (Tuple x y)
