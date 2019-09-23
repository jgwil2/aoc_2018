module Day3
(
  part1,
  part2
)
where

import Prelude

import Data.Array (concatMap, filter, group, init, length, partition, sort, unsafeIndex, (..))
import Data.Array.NonEmpty (NonEmptyArray, length) as NEA
import Data.Maybe (Maybe)
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
part2 text = show $ solution
  where
    words = getSafeArray $ init $ splitTextByNewline text
    rectsWithIds = map getRectsFromString words
    rects = map (\r -> snd r) rectsWithIds
    -- for all claims, check if they overlap with any others and build
    -- a list of every overlapping claim for each claim
    claimsWithOverlapping = map (\r -> (Tuple (fst r) (findOverlappingRects (snd r) rects))) rectsWithIds
    -- because we compare each claim against the total list, every claim
    -- will overlap with at least one other claim (itself)
    -- the claim that only overlaps with itself is the solution
    solution = filter (\v -> length (snd v) == 1) claimsWithOverlapping

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

-- cf. https://stackoverflow.com/questions/306316/determine-if-two-rectangles-overlap-each-other
-- note that the solution given above is for a standard cartesian plane;
-- since in our case y increases from top to bottom, the comparators for
-- y coordinates are reversed
rectsOverlap :: Rect -> Rect -> Boolean
rectsOverlap r s = r.upperLeft.x < s.lowerRight.x && r.lowerRight.x > s.upperLeft.x
  && r.upperLeft.y < s.lowerRight.y && r.lowerRight.y > s.upperLeft.y

-- given a rectangle and an array of rectangles, find all rectangles in
-- the array that overlap with the given rectangle
findOverlappingRects :: Rect -> Array Rect -> Array Rect
findOverlappingRects r rs = filter (rectsOverlap r) rs
