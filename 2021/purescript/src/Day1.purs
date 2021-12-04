module Day1 (solve) where

import Prelude

import Data.Array (filter)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Utils as SUtils
import Data.Traversable (traverse)
import Partial.Unsafe (unsafeCrashWith)

solve :: String -> { pt1 :: Int, pt2 :: Int }
solve input =
  let
    numbers = 
      SUtils.lines input
        # filter (not String.null)
        # traverse Int.fromString
        # Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid input")
        # List.fromFoldable
  in
  { pt1: countIncreases numbers
  , pt2: countIncreases $ group3 numbers
  }

defaultTail :: forall a. List a -> List a
defaultTail = List.tail >>> Maybe.fromMaybe List.Nil

group3 :: List Int -> List Int
group3 xs = zipWith3 (\a b c -> a + b + c) xs (defaultTail xs) (defaultTail $ defaultTail xs)

countIncreases :: List Int -> Int
countIncreases =
  toOrderingWithMatching
    >>> List.filter (eq GT)
    >>> List.length

toOrderingWithMatching :: forall a. Ord a => List a -> List Ordering
toOrderingWithMatching = case _ of
  x : y : xs -> compare y x : toOrderingWithMatching (y : xs)
  _ -> List.Nil

zipWith3 :: forall a. (a -> a -> a -> a) -> List a -> List a -> List a -> List a
zipWith3 f (x : xs) (y : ys) (z : zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = List.Nil
