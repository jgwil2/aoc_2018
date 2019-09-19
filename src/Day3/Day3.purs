module Day3
(
  part1,
  part2
)
where

import Prelude

import Data.Array (all, concatMap, elem, find, group, init, length, partition, sort, unsafeIndex, (..))
import Data.Array.NonEmpty (NonEmptyArray, head, length) as NEA
import Data.Foldable (class Foldable)
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
part2 text = show $ length rects
  where
    words = getSafeArray $ init $ splitTextByNewline text
    rects = map getRectsFromString words
    -- TODO for all rectangles, check if they overlap with any others

type ID = Int

type Claim = {
  id :: ID,
  left :: Int,
  top :: Int,
  width :: Int,
  height :: Int
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

data Rect = Rect {
  upperLeft :: Point,
  upperRight :: Point,
  lowerLeft :: Point,
  lowerRight :: Point
}

instance showRect :: Show Rect where
  show (Rect r) = "(Rect " <> show r.upperLeft <> ", " <> show r.upperRight
                  <> ",\r\n" <> show r.lowerLeft <> ", " <> show r.lowerRight

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
      tupleToPoint (Tuple x y) = Point { x: x, y: y }
  in
      map tupleToPoint $ cartesianProd xs ys

cartesianProd :: ∀ getNonOverlappingClaimId a b. Bind getNonOverlappingClaimId => Applicative getNonOverlappingClaimId => getNonOverlappingClaimId a -> getNonOverlappingClaimId b -> getNonOverlappingClaimId (Tuple a b)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure $ (Tuple x y)

getRectFromClaim :: Claim -> Rect
getRectFromClaim claim =
  Rect {
    upperLeft: Point {
      x: claim.left,
      y: claim.top
    },
    upperRight: Point {
      x: claim.left + claim.width,
      y: claim.top
    },
    lowerLeft: Point {
      x: claim.left,
      y: claim.top + claim.height
    },
    lowerRight: Point {
      x: claim.left + claim.width,
      y: claim.top + claim.height
    }
  }

getRectsFromString :: String -> Rect
getRectsFromString = getRectFromClaim <<< getClaimFromArray <<<
  map getSafeString <<< getSafeArrayfromNonEmpty <<< matchRecord
