module Test.Day1 where

import Prelude

import Data.Array (intercalate)
import Day1 (solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Spec Unit
main = do
  describe "Day 1" do
    describe "solve" do
      it "should return the correct result" do
        let
          input = toTextInput [ 199, 200, 208, 210, 200, 207, 240, 269, 260, 263 ]
        solve input `shouldEqual` { pt1: 7, pt2: 5 }

toTextInput :: Array Int -> String
toTextInput = map show >>> intercalate "\n" >>> flip append "\n"
