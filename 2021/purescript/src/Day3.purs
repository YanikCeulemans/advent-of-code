module Day3 where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Utils as SUtils
import Partial.Unsafe (unsafeCrashWith)

type DiagnosticReport =
  { gammaRate :: Int
  , epsilonRate :: Int
  }

type LifeSupportMetrics =
  { o2GeneratorRating :: Int
  , co2ScrubberRating :: Int
  }

solve :: String -> { pt1 :: Int, pt2 :: Int }
solve input =
  let
    bits =
      SUtils.lines input
        # Array.filter (not <<< String.null)
        # map bitsFromString
    pt1 =
      bits
        # calculateGammaRate
        # calculateDiagnosticReport
    pt2 =
      bits
        # calculateLifeSupportMetrics
  in
    { pt1: pt1.gammaRate * pt1.epsilonRate
    , pt2: pt2.o2GeneratorRating * pt2.co2ScrubberRating
    }

data Bit = Zero | One

derive instance Eq Bit

type Bits = Array Bit

flipBit :: Bit -> Bit
flipBit = case _ of
  One -> Zero
  Zero -> One

unsafeCharToBit :: String -> Bit
unsafeCharToBit = case _ of
  "0" -> Zero
  "1" -> One
  s -> unsafeCrashWith $ "invalid input, expected bits to be either 1 or 0, but got " <> s

bitsFromString :: String -> Bits
bitsFromString = SUtils.toCharArray >>> map unsafeCharToBit

arrayTranspose :: forall f a. Foldable f => f (f a) -> Array (Array a)
arrayTranspose =
  List.fromFoldable
    >>> map List.fromFoldable
    >>> List.transpose
    >>> Array.fromFoldable
    >>> map Array.fromFoldable

calculateGammaRate :: Array Bits -> Array Bit
calculateGammaRate = arrayTranspose >>> map selectMostCommon

selectMostCommon :: Bits -> Bit
selectMostCommon =
  List.foldl go { ones: 0, zeroes: 0 }
    >>> selectGreaterCount
  where
  go r@{ ones, zeroes } = case _ of
    One -> r { ones = ones + 1 }
    Zero -> r { zeroes = zeroes + 1 }

  selectGreaterCount { ones, zeroes }
    | ones >= zeroes = One
    | otherwise = Zero

calculateDiagnosticReport :: Bits -> DiagnosticReport
calculateDiagnosticReport bits =
  { gammaRate: bitsToInt bits
  , epsilonRate: bitsToInt $ map flipBit bits
  }

bitsToInt :: Bits -> Int
bitsToInt bits =
  Array.reverse bits
    # foldlWithIndex go 0
  where
  go i acc = case _ of
    One -> (Int.pow 2 i) * 1 + acc
    Zero -> acc

calculateLifeSupportMetrics :: Array Bits -> LifeSupportMetrics
calculateLifeSupportMetrics bitss =
  let
    toLifeSupportMetric = case _ of
      [ x ] -> bitsToInt x
      _ -> unsafeCrashWith "incorrect list lengths"
  in
    { o2GeneratorRating:
        calcLsMetric O2Generator 0 bitss
          # toLifeSupportMetric
    , co2ScrubberRating:
        calcLsMetric Co2Scrubber 0 bitss
          # toLifeSupportMetric
    }

calcLsMetric :: LsRating -> Int -> Array Bits -> Array Bits
calcLsMetric lsRating index = case _ of
  result@[ _ ] -> result
  bitss ->
    bitss
      # getRelevantBits index
      # getCriteriumIndices lsRating
      # map (unsafeIndex bitss)
      # calcLsMetric lsRating (index + 1)

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex xs index =
  Array.index xs index
    # Maybe.fromMaybe' (\_ -> unsafeCrashWith $ "index out of bounds: " <> show index)

getRelevantBits :: Int -> Array Bits -> Bits
getRelevantBits index = arrayTranspose >>> flip unsafeIndex index

data LsRating = O2Generator | Co2Scrubber

getCriteriumIndices :: LsRating -> Bits -> Array Int
getCriteriumIndices lsRating bits =
  let
    flipByLsRating bit =
      case lsRating of
        O2Generator -> bit
        Co2Scrubber -> flipBit bit

    mostCommon =
      selectMostCommon bits
        # flipByLsRating

    toIndex index bit
      | bit == mostCommon = Just index
      | otherwise = Nothing
  in
    Array.mapWithIndex toIndex bits
      # Array.catMaybes
