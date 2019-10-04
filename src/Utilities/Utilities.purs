module Utilities where

import Prelude

import Data.Array (init)
import Data.Array.NonEmpty (NonEmptyArray, toArray) as NEA
import Data.Int (fromString)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))

splitTextByNewline :: String -> Array String
splitTextByNewline s = getSafeArray $ init $ split (Pattern "\n") s

getSafeString :: Maybe String -> String
getSafeString (Just x) = x
getSafeString Nothing = ""

getSafeNum :: Maybe Int -> Int
getSafeNum (Just x) = x
getSafeNum Nothing = 0

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString

getSafeArray :: ∀ a. Maybe (Array a) -> Array a
getSafeArray (Just xs) = xs
getSafeArray Nothing = []

getSafeArrayfromNonEmpty :: ∀ a. Maybe (NEA.NonEmptyArray a) -> Array a
getSafeArrayfromNonEmpty (Just xs) = NEA.toArray xs
getSafeArrayfromNonEmpty Nothing = []

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

-- TODO find out how this works!
arrayToList :: forall a. Array a -> List a
arrayToList = fromFoldable

getSafeList :: forall a. Maybe (List a) -> List a
getSafeList (Just xs) = xs
getSafeList Nothing = Nil
