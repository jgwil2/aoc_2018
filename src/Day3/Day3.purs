module Day3
(
  part1,
  part2
)
where

import Prelude

import Data.Array (all, concatMap, elem, find, group, init, length, partition, sort, unsafeIndex, (..))
import Data.Array.NonEmpty (head, length, toArray) as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

part1 :: String -> String
part1 text = show $ length $ repeatedPoints.yes
  where
    words = getSafeArray $ init $ splitTextByNewline text
    totalPoints = concatMap getPointsFromString words
    groupedPoints = group $ sort totalPoints
    repeatedPoints = partition (\x -> 1 /= NEA.length x) groupedPoints

part2 :: String -> String
part2 text = show $ getNonOverlappingClaimId claimsWithId (map NEA.head repeatedPoints.no)
  where
    words = getSafeArray $ init $ splitTextByNewline text
    claimsWithId = map getPointsAndIdFromString words
    totalPoints = concatMap getPointsFromString words
    groupedPoints = group $ sort totalPoints
    repeatedPoints = partition (\x -> 1 /= NEA.length x) groupedPoints

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

getSafeArray :: ∀ a. Maybe (Array a) -> Array a
getSafeArray (Just xs) = xs
getSafeArray Nothing = []

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

cartesianProd :: ∀ getNonOverlappingClaimId a b. Bind getNonOverlappingClaimId => Applicative getNonOverlappingClaimId => getNonOverlappingClaimId a -> getNonOverlappingClaimId b -> getNonOverlappingClaimId (Tuple a b)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure $ (Tuple x y)

-- get all points by claim w/ id, then get all points that are repeated,
-- then find the claim which contains only non-repeated points
getPointsAndIdFromClaim :: Claim -> (Tuple ID (Array Point))
getPointsAndIdFromClaim c = (Tuple c.id (getPointsFromClaim c))

getPointsAndIdFromString :: String -> Tuple Int (Array Point)
getPointsAndIdFromString = getPointsAndIdFromClaim <<< getClaimFromArray
  <<< map getSafeString <<< getSafeArrayfromNonEmpty <<< matchRecord

getNonOverlappingClaimId :: Array (Tuple Int (Array Point)) -> Array Point -> Maybe Int
getNonOverlappingClaimId cs ps = map fst $ find (\c -> containsOnly (snd c) ps) cs

containsOnly :: forall f g a. Foldable g => Foldable f => Eq a => g a -> f a -> Boolean
containsOnly xs ys = all (\x -> elem x ys) xs
