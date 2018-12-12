module Utilities where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))

splitTextByNewline :: String -> Array String
splitTextByNewline = split (Pattern "\n")

getSafeString :: Maybe String -> String
getSafeString (Just x) = x
getSafeString Nothing = ""

getSafeNum :: Maybe Int -> Int
getSafeNum (Just x) = x
getSafeNum Nothing = 0

getSafeArray :: ∀ a. Maybe (Array a) -> Array a
getSafeArray (Just xs) = xs
getSafeArray Nothing = []

fromStringSafe :: String -> Int
fromStringSafe = getSafeNum <<< fromString

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