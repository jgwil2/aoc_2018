module Day3
(
  part1,
  part2
)
where

import Prelude

import Data.Array (all, concatMap, elem, filter, find, group, init, length, partition, sort, unsafeIndex, (..))
import Data.Array.NonEmpty (NonEmptyArray, head, length) as NEA
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe, isJust, isNothing)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Utilities (fromStringSafe, getSafeArray, getSafeArrayfromNonEmpty, getSafeString, splitTextByNewline)

part1 :: String -> String
part1 text = show $ length $ repeatedPoints.yes
  where
    words = getSafeArray $ init $ splitTextByNewline text
    totalPoints = concatMap getPointsFromString words
    groupedPoints = group $ sort totalPoints
    repeatedPoints = partition (\x -> 1 /= NEA.length x) groupedPoints

part2 :: String -> String
part2 text = show $ length sol
  where
    words = getSafeArray $ init $ splitTextByNewline text
    rectsWithIds = map getRectsFromString words
    rects = map (\r -> snd r) rectsWithIds
    -- TODO for all rectangles, check if they overlap with any others
    sol = filter (\v -> isJust $ snd v) $ map (\r -> (Tuple (fst r) (findNonOverlappingRect (snd r) rects))) rectsWithIds

type ID = Int

type Claim = {
  id :: ID,
  left :: Int,
  top :: Int,
  width :: Int,
  height :: Int
}

type Point = {
  x :: Int,
  y :: Int
}

type Rect = {
  upperLeft :: Point,
  upperRight :: Point,
  lowerLeft :: Point,
  lowerRight :: Point
}

recordPattern :: Regex
recordPattern = unsafeRegex "^#([0-9]*) @ ([0-9]*),([0-9]*): ([0-9]*)x([0-9]*)$" noFlags

matchRecord :: String -> Maybe (NEA.NonEmptyArray (Maybe String))
matchRecord = match recordPattern

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

getPointsFromClaim :: Claim -> Array Point
getPointsFromClaim claim =
  let xs = claim.left..(claim.left + claim.width - 1)
      ys = claim.top..(claim.top + claim.height - 1)
      tupleToPoint :: (Tuple Int Int) -> Point
      tupleToPoint (Tuple x y) = { x: x, y: y }
  in
      map tupleToPoint $ cartesianProd xs ys

cartesianProd :: ∀ getNonOverlappingClaimId a b. Bind getNonOverlappingClaimId => Applicative getNonOverlappingClaimId => getNonOverlappingClaimId a -> getNonOverlappingClaimId b -> getNonOverlappingClaimId (Tuple a b)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure $ (Tuple x y)

getRectFromClaim :: Claim -> Rect
getRectFromClaim claim =
  {
    upperLeft: {
      x: claim.left,
      y: claim.top
    },
    upperRight: {
      x: claim.left + claim.width,
      y: claim.top
    },
    lowerLeft: {
      x: claim.left,
      y: claim.top + claim.height
    },
    lowerRight: {
      x: claim.left + claim.width,
      y: claim.top + claim.height
    }
  }

getRectsFromString :: String -> (Tuple ID Rect)
getRectsFromString = (\c -> (Tuple c.id (getRectFromClaim c))) <<< getClaimFromArray <<<
  map getSafeString <<< getSafeArrayfromNonEmpty <<< matchRecord

rectsOverlap :: Rect -> Rect -> Boolean
rectsOverlap r s = r.lowerRight.y > s.upperLeft.y && r.lowerRight.x > s.upperLeft.x

-- given a rectangle and an array of rectangles, find a rectangle in the
-- array that does not overlap with the given rectangle
findNonOverlappingRect :: Rect -> Array Rect -> Maybe Rect
findNonOverlappingRect r rs = find (not rectsOverlap r) rs
